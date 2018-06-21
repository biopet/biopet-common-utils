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

import java.io.{File, PrintWriter}

import scala.collection.mutable.ListBuffer
import scala.util.Properties.lineSeparator

/**
  * This object contains functions that can be used for writing markdown and html files
  * for documentation purposes.
  */
object Documentation {

  /**
    * Returns a HTML table.
    * @param headers A list of strings that will make up the header.
    * @param body A list of lists of strings. Each list of strings is a row.
    *             Rows should be of equal length to the headers.
    * @return A HTML table as a string. The table contains newlines and indentation for readability.
    */
  def htmlTable(headers: List[String], body: List[List[String]]): String = {

    // Validate that all rows have a length equal to the header
    body.foreach(row =>
      require(
        row.length == headers.length,
        "Number of items in each row should be equal number of items in header."))

    val table = new StringBuffer()

    table.append("<table class=\"table\">\n")
    table.append(
      headers.mkString("\t<thead>\n\t\t<tr>\n\t\t\t<th>",
                       "</th>\n\t\t\t<th>",
                       "</th>\n\t\t</tr>\n\t</thead>\n"))
    table.append("\t<tbody>\n")
    for (row <- body) {
      table.append(
        row.mkString("\t\t<tr>\n\t\t\t<td>",
                     "</td>\n\t\t\t<td>",
                     "</td>\n\t\t</tr>\n"))
    }
    table.append("\t</tbody>\n")
    table.append("</table>\n")
    table.toString.replace("\t", "  ")
  }

  /**
    * Generates a Markdown file from a list of chapters (heading, content) tuples.
    * @param contents A list of (string, string) tuples, where the first string is the title and the other the content.
    * @param outputFile The output file to which the markdown file is written.
    */
  def contentsToMarkdown(
      contents: List[(String, String)],
      outputFile: File
  ): Unit = {
    outputFile.getParentFile.mkdirs()
    val fileWriter = new PrintWriter(outputFile)
    for ((head, content) <- contents) {
      fileWriter.println(head)
      fileWriter.println()
      fileWriter.println(content)
      fileWriter.println()
    }
    fileWriter.close()
  }

  /**
    * Generates a htmlPage that redirects automatically to the link provided.
    * @param outputFile The file that will contain the redirect, for example: some_dir/index.html
    * @param link The file to redirect to, for example: ../index.html
    * @param title The title of the page.
    * @param redirectText If javascript does not work, this link text is displayed.
    */
  def htmlRedirector(
      outputFile: File,
      link: String,
      title: String = "Project Documentation",
      redirectText: String = "Go to the project documentation"
  ): Unit = {
    val fileWriter = new PrintWriter(outputFile)
    val redirectHtml: String =
      s"""<!DOCTYPE html>
         |<html lang="en">
         |<head>
         |    <meta charset="UTF-8">
         |    <title>${title}</title>
         |    <script language="JavaScript">
         |        <!--
         |        function doRedirect()
         |        {
         |            window.location.replace("${link}");
         |        }
         |        doRedirect();
         |        //-->
         |    </script>
         |</head>
         |<body>
         |<a href="${link}">${redirectText}
         |</a>
         |</body>
         |</html>
       """.stripMargin
    fileWriter.print(redirectHtml)
    fileWriter.close()
  }

  /**
    * Split a list of strings at a certain element.
    *
    * @param stringList The list of strings
    * @param splitter   Function that determines whether the list should be split at this line.
    * @return A list of lists of string.
    */
  def splitStringList(stringList: List[String],
                      splitter: String => Boolean): List[List[String]] = {
    stringList
      .foldLeft(ListBuffer[ListBuffer[String]]()) {
        case (result, line) if splitter(line) || result.lastOption.isEmpty =>
          result += ListBuffer(line)
        case (result, line) =>
          result.lastOption.foreach(_ += line)
          result
      }
      .map(buffer => buffer.toList)
      .toList
  }

  /**
    * Extracts a chapter from a markdown string
    *
    * @param markdown the input string
    * @param chapter  the chapter heading. (without #)
    * @return The chapter including header and subheaders as a string.
    */
  def markdownExtractChapter(markdown: String,
                             chapter: String,
                             includeHeader: Boolean = true): String = {
    // Regex is anchored with ^pattern$ by default.
    // Matches any number of #'s, a whitespace (tab or space) that does not have to exist and the chapter.
    val chapterRegex =
      s"(#+)([\\t ]*)($chapter)".r("hashtags", "whitespace", "heading")
    val headingDepth = chapterRegex
      .findFirstMatchIn(markdown)
      .map(_.group("hashtags").length)
      .getOrElse(
        throw new IllegalStateException(s"Chapter not found: $chapter"))

    // General matcher that returns true if the line is
    // a. a markdown chapter (starting with #)
    // b. Starting with a maximum of headingDepth hashtags.
    // If the maximum dept is exceeded (another # was found) then retuns false.
    // This way we can split the markdown at a desired depth.
    lazy val headerSplitRegex =
      s"^(#{1,$headingDepth})[^#]([\\t ]*)(.*)$$".r

    def matcher(string: String): Boolean = {
      headerSplitRegex.findFirstMatchIn(string).isDefined
    }

    val text = markdown.split(lineSeparator).toList
    val chapters = splitStringList(text, matcher)

    // In a list of lists of string. Find the list that starts with
    // the string that matches the chapter regex.
    // if so return that chapter by concatenating the lines using the
    // lineSeparator. Optionally drop the first line if the header
    // should not be included.
    // Throw an error if the chapter is not found. This should not occur
    // since the chapter's existence has already been evaluated before
    // this point.
    chapters
      .find(lines =>
        lines.headOption.exists(chapterRegex.findFirstMatchIn(_).isDefined))
      .map(lines => if (includeHeader) lines else lines.drop(1))
      .map(_.mkString(lineSeparator))
      .getOrElse(throw new IllegalStateException(
        "Cannot validate correct chapter after parsing. " +
          "Please contact application maintainers."))

  }

}
