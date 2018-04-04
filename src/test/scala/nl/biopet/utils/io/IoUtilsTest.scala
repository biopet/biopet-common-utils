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

import java.io.{File, FileNotFoundException, PrintWriter, IOException}
import java.net.URL
import java.nio.file.Files

import scala.util.matching.Regex
import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

import scala.io.Source

/**
  * Created by pjvanthof on 05/05/16.
  */
class IoUtilsTest extends BiopetTest {

  def createTempTestFile(file: File): File = {
    file.getParentFile.mkdirs()
    val writer = new PrintWriter(file)
    writer.println("test")
    writer.close()
    file.deleteOnExit()
    file
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
      val file = createTempTestFile(new File(tempDir1, x))
      file.setExecutable(true)
      new File(tempDir2, x) shouldNot exist
    }
    tempDir2.delete()
    tempDir2.createNewFile()
    intercept[IOException] {
      copyDir(tempDir1, tempDir2)
    }.getMessage shouldBe s"${tempDir2.getAbsolutePath} is a file, not a directory"
    tempDir2.delete()
    tempDir2.mkdirs()
    copyDir(tempDir1, tempDir2)
    relativePaths.foreach { x =>
      val file = new File(tempDir2, x)
      file should exist
      file.canExecute shouldEqual true
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

  @Test
  def testWriteStringToFile(): Unit = {
    val string = "testing testerdetest test"
    val file = File.createTempFile("stringtofile.", ".txt")
    file.deleteOnExit()
    stringToFile(string, file)
    Source.fromFile(file).mkString shouldEqual (string + "\n")
  }

  @Test
  def testSha256Sum(): Unit = {
    // Taken the README from Biopet 0.9.0. Small, link should be stable
    val downloadLink: URL =
      new URL(
        "https://raw.githubusercontent.com/biopet/biopet/be7838f27f3cad9f80191d92a4a795c34d1ae092/README.md")
    getSha256SumFromDownload(downloadLink) shouldBe Some(
      "186e801bf3cacbd564b4ec00815352218038728bd6787b71f65db474a3588901")
    getSha256SumFromDownload(new URL(downloadLink.toString + "nonsense")) shouldBe None
  }

  @Test
  def testFindFile(): Unit = {
    val tempDir = Files.createTempDirectory("test").toFile
    tempDir.deleteOnExit()
    val relativePaths: List[String] = List(
      "test1.txt",
      "test2.txt",
      "dir1" + File.separator + "test1.txt",
      "dir1" + File.separator + "test2.txt",
      "dir2" + File.separator + "test1.txt",
      "dir2" + File.separator + "test2.txt"
    )
    val allFiles: Seq[File] = relativePaths.map(file =>
      createTempTestFile(new File(tempDir, file)))

    findFile(tempDir,recursive=true).toSet shouldBe allFiles.toSet
    findFile(tempDir).toSet should not be allFiles.toSet
    val twoFiles = List("test2.txt",
      "dir1" + File.separator + "test2.txt",
      "dir2" + File.separator + "test2.txt"
    ).map(file => new File(tempDir, file))
    findFile(tempDir, Some(new Regex(".*2\\.txt$")),recursive = true).toSet shouldBe twoFiles.toSet
    findFile(tempDir, Some(new Regex(".*2\\.txt$"))).toSet shouldBe Set(new File(tempDir, "test2.txt"))
    findFile(tempDir, Some(new Regex("^dir"))).toSet shouldBe Set(new File(tempDir,"dir1"), new File(tempDir,"dir2"))
    findFile(tempDir, Some(new Regex("^dir")), recursive = true).toSet shouldBe Set()


  }
}
