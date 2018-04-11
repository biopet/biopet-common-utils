/*
 * Copyright (c) 2014 Biopet
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package nl.biopet.utils.process

/*
The MIT License (MIT)
Copyright (c) 2016 j-keck <jhyphenkeck@gmail.com>
Permission is hereby granted, free of charge, to any person obtaining a copy of this
software and associated documentation files (the "Software"), to deal in the Software
without restriction, including without limitation the rights to use, copy, modify,
merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

import java.io.File

import nl.biopet.utils.Logging

import scala.collection.parallel.mutable.ParMap
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.sys.process.{Process, ProcessLogger}
import scala.util.Try

object Sys extends Sys {
  type ExitValue = Int
  type Stdout = String
  type Stderr = String

  type ExecResult = (ExitValue, Stdout, Stderr)

  trait AsyncExecResult {

    /**
      * @see [[scala.concurrent.Future#map]]
      */
    def map[T](f: ExecResult => T): Future[T]

    /**
      * @see [[scala.concurrent.Future#foreach]]
      */
    def foreach(f: ExecResult => Unit): Unit

    /**
      * @see [[scala.concurrent.Future#onComplete]]
      */
    def onComplete[T](pf: Try[ExecResult] => T): Unit

    /** cancels the running process */
    def cancel(): Unit

    /**
      * check if the process is still running
      * @return `true` if the process is already completed, `false` otherwise
      */
    def isRunning: Boolean

    /**
      * the underlying future
      * @return the future, in which the process runs
      */
    def get: Future[ExecResult]
  }

  type Cancelable = () => Unit

  case class ExecutionCanceled(msg: String) extends Exception(msg)
}

trait Sys {
  import Sys._

  private val cache: ParMap[Seq[String], AsyncExecResult] = ParMap()

  var maxRunningProcesses: Int = 5

  def exec(cmd: String,
           cwd: Option[File],
           env: Map[String, String]): ExecResult =
    exec(cmd.split(" "), cwd, env)

  def exec(cmd: String): ExecResult =
    exec(cmd.split(" "))

  def exec(cmd: String, cwd: Option[File]): ExecResult =
    exec(cmd.split(" "), cwd)

  def exec(cmd: String, env: Map[String, String]): ExecResult =
    exec(cmd.split(" "), env = env)

  /**
    * executes the cmd and blocks until the command exits.
    * @param cmd The command to be executed.
    * @param cwd Current Working Directory for the process
    * @param env Extra environment variables for the process
    * @return {{{(ExitValue, Stdout, Stderr)}}}
    *         <pre>if the executable is unable to start, (-1, "", stderr) are returned</pre>
    */
  def exec(cmd: Seq[String],
           cwd: Option[File] = None,
           env: Map[String, String] = Map()): ExecResult = {
    val stdout = new OutputSlurper
    val stderr = new OutputSlurper

    Try {
      val proc =
        Process(cmd, cwd, env.toList: _*).run(
          ProcessLogger(stdout.appendLine, stderr.appendLine))
      proc.exitValue()
    }.map((_, stdout.get, stderr.get))
      .recover {
        case t => (-1, "", t.getMessage)
      }
      .get
  }

  def execAsync(cmd: String, cwd: Option[File], env: Map[String, String])(
      implicit ec: ExecutionContext): AsyncExecResult =
    execAsync(cmd.split(" "), cwd, env)(ec)

  def execAsync(cmd: String)(implicit ec: ExecutionContext): AsyncExecResult =
    execAsync(cmd.split(" "))(ec)

  def execAsync(cmd: String, cwd: Option[File])(
      implicit ec: ExecutionContext): AsyncExecResult =
    execAsync(cmd.split(" "), cwd)(ec)

  def execAsync(cmd: String, env: Map[String, String])(
      implicit ec: ExecutionContext): AsyncExecResult =
    execAsync(cmd.split(" "), env = env)(ec)

  /**
    * executes the cmd asynchronous
    * @see scala.concurrent.Future.map
    * @param cmd The command to be executed.
    * @param cwd Current Working Directory for the process
    * @param env Extra environment variables for the process
    * @param ec
    * @return [[AsyncExecResult]]
    */
  def execAsync(cmd: Seq[String],
                cwd: Option[File] = None,
                env: Map[String, String] = Map())(
      implicit ec: ExecutionContext): AsyncExecResult = {
    while (cache.size >= maxRunningProcesses) {
      for ((cmd, c) <- cache.toList) {
        val results = Option(c)
        if (!results.forall(_.isRunning)) try {
          cache -= cmd
        } catch {
          case _: NullPointerException =>
        } else
          try {
            results.foreach(x => Await.ready(x.get, Duration.fromNanos(100000)))
          } catch {
            case _: TimeoutException =>
          }
      }
    }
    val results = new AsyncExecResult {
      val (fut, cancelFut) = runAsync(cmd, cwd, env)

      override def map[T](f: ExecResult => T): Future[T] = fut.map(f)

      override def foreach(f: ExecResult => Unit): Unit = fut.foreach(f)

      override def onComplete[T](pf: Try[ExecResult] => T): Unit =
        fut.onComplete(pf)

      override def cancel(): Unit = cancelFut()

      override def isRunning: Boolean = !fut.isCompleted

      override def get: Future[ExecResult] = fut
    }
    cache += cmd -> results
    results
  }

  // helper for 'execAsync' - runs the given cmd asynchronous.
  // returns a tuple with: (the running process in a future, function to cancel the running process)
  private def runAsync(cmd: Seq[String],
                       cwd: Option[File] = None,
                       env: Map[String, String] = Map())(
      implicit ec: ExecutionContext): (Future[ExecResult], Cancelable) = {
    val p = Promise[ExecResult]

    val stdout = new OutputSlurper
    val stderr = new OutputSlurper

    // start the process
    val proc =
      Process(cmd, cwd, env.toList: _*)
        .run(ProcessLogger(stdout.appendLine, stderr.appendLine))
    p.tryCompleteWith(
      Future(proc.exitValue).map(c => (c, stdout.get, stderr.get)))

    val cancel = () => {
      p.tryFailure {
        Logging.logger.error("stdout: " + stdout.get)
        Logging.logger.error("stderr: " + stderr.get)
        ExecutionCanceled(s"Process: '${cmd.mkString(" ")}' canceled")
      }
      proc.destroy()
    }

    (p.future, cancel)
  }

  class OutputSlurper {
    private val sb = new StringBuilder

    def append(s: String): Unit = sb.append(s)

    def appendLine(s: String): Unit = append(s + "\n")

    def get: String = sb.toString
  }
}
