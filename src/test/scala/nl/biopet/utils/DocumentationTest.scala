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

package nl.biopet.utils

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test
import java.io.File

import scala.io.Source
import scala.util.Properties.lineSeparator
import Documentation._

class DocumentationTest extends BiopetTest {
  @Test
  def testTableMethod(): Unit = {
    the[java.lang.IllegalArgumentException] thrownBy {
      htmlTable(
        List("Column1", "Column2"),
        List(
          List("1", "2"),
          List("a", "b", "c")
        )
      )
    } should have message "requirement failed: Number of items in each row should be equal number of items in header."
    val table: String = htmlTable(List("Column1", "Column2"),
                                  List(List("1", "2"), List("a", "b")))
    table should contain
    """<table>
      |  <thead>
      |    <tr>
      |      <th>Column1</th>
      |      <th>Column2</th>
      |    </tr>
      |  </thead>
      |  <tbody>
      |    <tr>
      |      <td>1</td>
      |      <td>2</td>
      |    </tr>
      |    <tr>
      |      <td>a</td>
      |      <td>b</td>
      |    </tr>
      |  </tbody>
      |</table>
    """.stripMargin
  }

  @Test
  def testContentToFile(): Unit = {
    val testMd = File.createTempFile("test.", ".md")
    contentsToMarkdown(
      List(
        ("# Test", "Lorem ipsum dolor sit amet, consectetur adipiscing elit")
      ),
      testMd)
    testMd should exist
    val reader = Source.fromFile(testMd)
    reader.mkString should include(
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit")
  }

  @Test
  def testHtmlRedirector(): Unit = {
    val testRedirect = File.createTempFile("test.", ".html")
    htmlRedirector(outputFile = testRedirect,
                   link = "bla/index.html",
                   title = "Project X",
                   redirectText = "Click here for X")

    testRedirect should exist
    val reader = Source.fromFile(testRedirect)
    val htmlPage = reader.mkString

    htmlPage should contain
    """<!DOCTYPE html>
      |<html lang="en">
      |<head>
      |    <meta charset="UTF-8">
      |    <title>Project X</title>
      |    <script language="JavaScript">
      |        <!--
      |        function doRedirect()
      |        {
      |            window.location.replace("bla/index.html");
      |        }
      |        doRedirect();
      |        //-->
      |    </script>
      |</head>
      |<body>
      |<a href="bla/index.html">Click here for X
      |</a>
      |</body>
      |</html>""".stripMargin
  }

  @Test
  def testMarkdownExtractChapter(): Unit = {
    val markdown: String = Source
      .fromFile(resourceFile("/test.md"))
      .getLines()
      .mkString("\n")
    val n = lineSeparator
    markdownExtractChapter(markdown, "Onedotone") shouldBe """## Onedotone
                                                             |1.1
                                                             |
                                                             |### Onedotonedotone
                                                             |1.1.1
                                                             |""".stripMargin
    markdownExtractChapter(markdown, "One") shouldBe
      """# One
        |1
        |
        |## Onedotone
        |1.1
        |
        |### Onedotonedotone
        |1.1.1
        |
        |## Onedottwo
        |1.2
        |
        |## Onedotthree
        |1.3
        |
        |## Onedotfour
        |1.4
        |""".stripMargin
    markdownExtractChapter(markdown, "Onedotonedotone") shouldBe
      """### Onedotonedotone
        |1.1.1
        |""".stripMargin
    markdownExtractChapter(markdown, "Onedotonedotone", includeHeader = false) shouldBe
      """|1.1.1
         |""".stripMargin
  }
  @Test
  def testSplitStringList(): Unit = {
    val a = List("a", "ab", "bc", "ac")
    splitStringList(a, x => x.startsWith("a")) shouldBe List(List("a"),
                                                             List("ab", "bc"),
                                                             List("ac"))
    val b = List("c", "c", "a", "ab", "bc", "ac")
    splitStringList(b, x => x.startsWith("a")) shouldBe List(List("c", "c"),
                                                             List("a"),
                                                             List("ab", "bc"),
                                                             List("ac"))
  }
}
