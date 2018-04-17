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

package nl.biopet.utils.rscript

import java.io.{File, FileOutputStream}

import nl.biopet.utils.Logging
import nl.biopet.utils.process.Sys

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}
import scala.sys.process.ProcessLogger

/**
  * Trait for rscripts, can be used to execute rscripts locally
  *
  * Created by pjvanthof on 13/09/15.
  */
trait Rscript extends Logging {
  protected def scriptPath: String

  protected def rscriptExecutable: String = "Rscript"

  if (!Rscript.alreadyCopied.contains(scriptPath)) {
    val rScript: File = File.createTempFile(scriptPath, ".R")
    rScript.deleteOnExit()

    val is = getClass.getResourceAsStream(scriptPath)
    val os = new FileOutputStream(rScript)

    org.apache.commons.io.IOUtils.copy(is, os)
    os.close()
    is.close()
    Rscript.alreadyCopied += scriptPath -> rScript
  }

  /** This is the default implementation, to add arguments override this */
  def cmd: Seq[String] =
    Seq(rscriptExecutable, Rscript.alreadyCopied(scriptPath).getAbsolutePath)

  /**
    * Execute rscript on local system
    * @param logger How to handle stdout and stderr
    */
  def runLocal(logger: ProcessLogger)(implicit ec: ExecutionContext): Unit = {
    Logging.logger.info("Running: " + cmd.mkString(" "))

    val results = Sys.execAsync(cmd)

    val (exitcode, stdout, stderr) =
      Await.result(results.map(x => (x._1, x._2, x._3)), Duration.Inf)

    Logging.logger.info("stdout:\n" + stdout + "\n")
    Logging.logger.info("stderr:\n" + stderr)

    Logging.logger.info(exitcode)
  }

  /**
    * Execute rscript on local system
    * Stdout and stderr will go to biopet logger
    */
  def runLocal()(implicit ec: ExecutionContext): Unit = {
    runLocal(ProcessLogger(Logging.logger.info(_)))
  }
}

object Rscript {
  protected val alreadyCopied: mutable.Map[String, File] = mutable.Map()
}
