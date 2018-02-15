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

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class SysTest extends BiopetTest {
  @Test
  def testCmd(): Unit = {
    val process = Sys.exec("echo bla")
    process._1 shouldBe 0
    process._2 shouldBe "bla\n"
    process._3 shouldBe ""
  }

  @Test
  def testCmdSeq(): Unit = {
    val process = Sys.exec(Seq("echo", "bla"))
    process._1 shouldBe 0
    process._2 shouldBe "bla\n"
    process._3 shouldBe ""
  }

  @Test
  def testCmdAsync(): Unit = {
    val process = Sys.execAsync("echo bla")
    val result = Await.result(process.get, Duration.Inf)
    result._1 shouldBe 0
    result._2 shouldBe "bla\n"
    result._3 shouldBe ""
  }

  @Test
  def testCmdSeqAsync(): Unit = {
    val process = Sys.execAsync(Seq("echo", "bla"))
    val result = Await.result(process.get, Duration.Inf)
    result._1 shouldBe 0
    result._2 shouldBe "bla\n"
    result._3 shouldBe ""
  }

  @Test
  def testMultiAsync(): Unit = {
    Sys.maxRunningProcesses = 1
    val process = Sys.execAsync(Seq("echo", "bla"))
    val process2 = Sys.execAsync(Seq("echo", "bla"))
    val process3 = Sys.execAsync(Seq("echo", "bla"))
    List(process, process2, process3).foreach { p =>
      val result = Await.result(p.get, Duration.Inf)
      result._1 shouldBe 0
      result._2 shouldBe "bla\n"
      result._3 shouldBe ""
    }
  }

  @Test
  def testCancel(): Unit = {
    Sys.maxRunningProcesses = 1
    val process = Sys.execAsync(Seq("echo", "bla"))
    process.cancel()
  }

}
