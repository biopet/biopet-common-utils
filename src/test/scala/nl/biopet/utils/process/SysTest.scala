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

import java.io.File
import java.nio.file.Files

import nl.biopet.test.BiopetTest
import nl.biopet.utils.process.Sys.{ExitValue, Stderr, Stdout}
import org.testng.annotations.Test

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class SysTest extends BiopetTest {
  @Test
  def testCmd(): Unit = {
    val process = Sys.execString("echo bla")
    process match {
      case (exit: ExitValue, stdout: Stdout, stderr: Stderr) =>
        exit shouldBe 0
        stdout shouldBe "bla\n"
        stderr shouldBe ""
    }
  }

  @Test
  def testCmdSeq(): Unit = {
    val process = Sys.exec(Seq("echo", "bla"))
    process match {
      case (exit: ExitValue, stdout: Stdout, stderr: Stderr) =>
        exit shouldBe 0
        stdout shouldBe "bla\n"
        stderr shouldBe ""
    }
  }

  @Test
  def testCmdAsync(): Unit = {
    val process = Sys.execAsyncString("echo bla")
    val result = Await.result(process.get, Duration.Inf)
    result match {
      case (exit: ExitValue, stdout: Stdout, stderr: Stderr) =>
        exit shouldBe 0
        stdout shouldBe "bla\n"
        stderr shouldBe ""
    }
  }

  @Test
  def testCmdSeqAsync(): Unit = {
    val process = Sys.execAsync(Seq("echo", "bla"))
    val result = Await.result(process.get, Duration.Inf)
    result match {
      case (exit: ExitValue, stdout: Stdout, stderr: Stderr) =>
        exit shouldBe 0
        stdout shouldBe "bla\n"
        stderr shouldBe ""
    }
  }

  @Test
  def testMultiAsync(): Unit = {
    Sys.maxRunningProcesses = 1
    val process = Sys.execAsync(Seq("echo", "bla"))
    val process2 = Sys.execAsync(Seq("echo", "bla"))
    val process3 = Sys.execAsync(Seq("echo", "bla"))
    List(process, process2, process3).foreach { p =>
      val result = Await.result(p.get, Duration.Inf)
      result match {
        case (exit: ExitValue, stdout: Stdout, stderr: Stderr) =>
          exit shouldBe 0
          stdout shouldBe "bla\n"
          stderr shouldBe ""
      }
    }
  }

  @Test
  def testCancel(): Unit = {
    Sys.maxRunningProcesses = 1
    val process = Sys.execAsync(Seq("echo", "bla"))
    process.cancel()
  }

  @Test
  def testCwd(): Unit = {
    val testDir = Files.createTempDirectory("test").toFile
    testDir.mkdirs()
    new File(testDir, "file1").createNewFile()
    new File(testDir, "file2").createNewFile()
    val processNormal = Sys.execString("ls", cwd = Some(testDir))
    val processAsync =
      Await.result(Sys.execAsyncString("ls", cwd = Some(testDir)).get,
                   Duration.Inf)
    for (process <- Seq(processNormal, processAsync)) {
      process match {
        case (exit: ExitValue, stdout: Stdout, stderr: Stderr) =>
          exit shouldBe 0
          stdout should include("file1")
          stdout should include("file2")
          stderr shouldBe ""
      }
    }
  }
  @Test
  def testEnv(): Unit = {
    val process =
      Sys.execString("printenv TEST", env = Map("TEST" -> "TestMessage"))
    process match {
      case (exit: ExitValue, stdout: Stdout, stderr: Stderr) =>
        exit shouldBe 0
        stdout shouldBe "TestMessage\n"
        stderr shouldBe ""
    }
  }

  def testEnvAsync(): Unit = {
    val process =
      Sys.execAsyncString("printenv TEST", env = Map("TEST" -> "TestMessage"))
    val result = Await.result(process.get, Duration.Inf)
    result match {
      case (exit: ExitValue, stdout: Stdout, stderr: Stderr) =>
        exit shouldBe 0
        stdout shouldBe "TestMessage\n"
        stderr shouldBe ""
    }
  }

}
