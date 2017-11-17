package nl.biopet.utils

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test
import java.io.File

import scala.io.Source

class MarkdownTest extends BiopetTest {
  @Test
  def testTableMethod(): Unit = {
    the [java.lang.IllegalArgumentException] thrownBy {
      Markdown.htmlTable(
        List("Column1", "Column2"),
        List(
          List("1","2"),
          List("a","b","c")
        )
      ) } should have message "requirement failed: Number of items in each row should be equal number of items in header."
    val table: String = Markdown.htmlTable(List("Column1", "Column2"),
    List(
      List("1","2"),
      List("a","b")))
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
    val testMd = File.createTempFile("test.",".md")
    Markdown.contentsToMarkdown(List(
    ("# Test",
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit")
  ), testMd
  )
  testMd should exist
  val reader = Source.fromFile(testMd)
  reader.mkString should include ("Lorem ipsum dolor sit amet, consectetur adipiscing elit")
  }

}