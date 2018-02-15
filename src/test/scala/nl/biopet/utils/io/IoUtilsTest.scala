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

package nl.biopet.utils.io

import java.io.{File, FileNotFoundException, PrintWriter}
import java.nio.file.Files

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

import scala.io.Source

/**
  * Created by pjvanthof on 05/05/16.
  */
class IoUtilsTest extends BiopetTest {

  def createTempTestFile(file: File): Unit = {
    file.getParentFile.mkdirs()
    val writer = new PrintWriter(file)
    writer.println("test")
    writer.close()
    file.deleteOnExit()
  }

  @Test
  def testCopyFile(): Unit = {
    val temp1 = File.createTempFile("test.", ".txt")
    temp1.deleteOnExit()
    val temp2 = File.createTempFile("test.", ".txt")
    temp2.deleteOnExit()
    createTempTestFile(temp1)
    copyFile(temp1, temp2)
    val reader = Source.fromFile(temp2)
    reader.getLines().toList shouldBe List("test")
    reader.close()
  }

  @Test
  def testResourceToFile(): Unit = {
    val temp1 = File.createTempFile("test.", ".fa")
    resourceToFile("/fake_chrQ.fa", temp1)
    temp1 should exist
    val reader = Source.fromFile(temp1)
    reader.mkString should include("CGCGAGCTCCTACCAGTCAACGTGATTGATCC")
  }

  @Test
  def testCopyFileNonExistingDir(): Unit = {
    val temp1 = File.createTempFile("test.", ".txt")
    val tempDir =
      new File(Files.createTempDirectory("test").toFile, "non-exist")
    tempDir.deleteOnExit()
    tempDir shouldNot exist
    val temp2 = new File(tempDir, "test.txt")
    createTempTestFile(temp1)
    intercept[FileNotFoundException] {
      copyFile(temp1, temp2)
    }
    copyFile(temp1, temp2, createDirs = true)
    val reader = Source.fromFile(temp2)
    reader.getLines().toList shouldBe List("test")
    reader.close()
  }

  @Test
  def testCopyDir(): Unit = {
    val tempDir1 = Files.createTempDirectory("test").toFile
    tempDir1.deleteOnExit()
    val tempDir2 = Files.createTempDirectory("test").toFile
    tempDir2.deleteOnExit()
    val relativePaths: List[String] = List(
      "test1.txt",
      "test2.txt",
      "dir1" + File.separator + "test1.txt",
      "dir1" + File.separator + "test2.txt",
      "dir2" + File.separator + "test1.txt",
      "dir2" + File.separator + "test2.txt"
    )
    relativePaths.foreach { x =>
      createTempTestFile(new File(tempDir1, x))
      new File(tempDir2, x) shouldNot exist
    }
    copyDir(tempDir1, tempDir2)
    relativePaths.foreach { x =>
      val file = new File(tempDir2, x)
      file should exist
      val reader = Source.fromFile(file)
      reader.getLines().toList shouldBe List("test")
      reader.close()
    }
  }

  @Test
  def testGetUncompressedFileName(): Unit = {
    getUncompressedFileName(new File("test.gz")) shouldBe "test"
  }

  @Test
  def testGetLinesFromFile(): Unit = {
    val file = File.createTempFile("test.", ".txt")
    file.deleteOnExit()
    val writer = new PrintWriter(file)
    writer.println("test")
    writer.close()
    getLinesFromFile(file) shouldBe List("test")
  }

  @Test
  def testWriteLinesToFile(): Unit = {
    val file = File.createTempFile("test.", ".txt")
    file.deleteOnExit()
    writeLinesToFile(file, List("test"))

    getLinesFromFile(file) shouldBe List("test")
  }
}
