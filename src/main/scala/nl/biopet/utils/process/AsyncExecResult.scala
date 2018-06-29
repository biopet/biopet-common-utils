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

import scala.concurrent.Future
import scala.util.Try

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
