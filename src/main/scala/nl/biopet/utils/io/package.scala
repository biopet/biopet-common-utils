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

import java.io._

import scala.io.Source
import org.apache.commons.io.IOUtils
import scala.util.matching.Regex
import java.net.URL
import scala.language.postfixOps
import com.roundeights.hasher.Implicits._

package object io {
  def copyFile(in: File,
               out: File,
               createDirs: Boolean = false,
               permissions: Boolean = true): Unit = {
    copyStreamToFile(new FileInputStream(in), out, createDirs)
    if (permissions) {
      out.setReadable(in.canRead)
      out.setWritable(in.canWrite)
      out.setExecutable(in.canExecute)
    }
  }

  /**
    * Finds a file in a specified directory.
    * @param dir directory to be searched
    * @param regex optional regex. Files matching this regex will be returned.
    *              If not specified all files are returned.
    * @param recursive if true also subdirectories are searched.
    *                  if false subdirectories are treated as files
    * @return all files in the directory
    *         (and subdirectories if recursive)
    *         (that match the regex if specified)
    */
  def listDirectory(dir: File,
                    regex: Option[Regex] = None,
                    recursive: Boolean = false): Seq[File] = {
    require(dir.isDirectory)
    val files = dir.listFiles()

    files.flatMap { file =>
      regex match {
        case _ if file.isDirectory && recursive =>
          listDirectory(file, regex, recursive)
        case Some(r) =>
          r.findFirstIn(file.getName) match {
            case Some(_) => Some(file)
            case _       => None
          }
        case _ => Some(file)
      }
    }
  }

  def copyStreamToFile(in: InputStream,
                       out: File,
                       createDirs: Boolean = false): Unit = {
    if (createDirs) out.getParentFile.mkdirs()
    val os = new FileOutputStream(out)

    IOUtils.copy(in, os)
    os.close()
    in.close()
  }

  /**
    * Converts a resource to a file
    * @param resource Which resource
    * @param outputFile The output file
    */
  def resourceToFile(resource: String, outputFile: File): Unit = {
    val source = getClass.getResourceAsStream(resource)
    copyStreamToFile(source, outputFile, createDirs = true)
  }

  /**
    * Copies contents of a directory to a new directory.
    * @param inputDir the input directory
    * @param externalDir the output directory
    */
  def copyDir(inputDir: File,
              externalDir: File,
              permissions: Boolean = true): Unit = {
    require(inputDir.isDirectory)
    if (externalDir.exists()) {
      if (!externalDir.isDirectory) {
        throw new IOException(
          s"${externalDir.getAbsolutePath} is a file, not a directory")
      }
    } else externalDir.mkdirs()
    for (srcFile <- inputDir.listFiles) {
      if (srcFile.isDirectory)
        copyDir(new File(inputDir, srcFile.getName),
                new File(externalDir, srcFile.getName))
      else {
        val newFile = new File(externalDir, srcFile.getName)
        copyFile(srcFile, newFile, permissions = permissions)
      }
    }
  }

  /**
    * Writes a string to a file
    * @param string the string
    * @param file the file
    */
  def stringToFile(string: String, file: File): Unit = {
    val writer = new PrintWriter(file)
    writer.println(string)
    writer.close()
  }

  /** Possible compression extensions to trim from input files. */
  val zipExtensions = Set(".gz", ".gzip", ".bzip2", ".bz", ".xz", ".zip")

  /**
    * Given a file object and a set of compression extensions, return the filename without any of the compression
    * extensions.
    *
    * Examples:
    *  - my_file.fq.gz returns "my_file.fq"
    *  - my_other_file.fastq returns "my_file.fastq"
    *
    * @param f Input file object.
    * @param exts Possible compression extensions to trim.
    * @return Filename without compression extension.
    */
  def getUncompressedFileName(f: File,
                              exts: Set[String] = zipExtensions): String =
    exts.foldLeft(f.getName) { (fname, ext) =>
      if (fname.toLowerCase.endsWith(ext)) fname.dropRight(ext.length)
      else fname
    }

  /** This return the contends of a file as a List[String] */
  def getLinesFromFile(file: File): List[String] = {
    val reader = Source.fromFile(file)
    val lines = reader.getLines().toList
    reader.close()
    lines
  }

  /** This writes a List[String] to a file */
  def writeLinesToFile(outputFile: File, lines: List[String]): Unit = {
    val writer = new PrintWriter(outputFile)
    lines.foreach(writer.println)
    writer.close()
  }

  /**
    * Calculates the sha256sum of a file that is downloaded from the URL
    * @param url the URL
    * @return the hex of the sha256sum.
    */
  def getSha256SumFromDownload(url: URL): Option[String] = {
    try {
      Some(url.openStream().sha256.hex)
    } catch {
      case e: java.io.FileNotFoundException => None
    }
  }
}
