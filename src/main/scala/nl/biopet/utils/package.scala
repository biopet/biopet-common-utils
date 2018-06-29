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

package nl.biopet

package object utils {

  /** Package version */
  // needs the Option here since the value is `null` when we run from an unpackaged JAR
  val Version: String =
    Option(getClass.getPackage.getImplementationVersion)
      .getOrElse("unpackaged")

  def textToSize(text: String): Long = {
    text.lastOption match {
      case Some('g') | Some('G') =>
        ((1L << 30) * text.stripSuffix("g").stripSuffix("G").toDouble).toLong
      case Some('m') | Some('M') =>
        ((1L << 20) * text.stripSuffix("m").stripSuffix("M").toDouble).toLong
      case Some('k') | Some('K') =>
        ((1L << 10) * text.stripSuffix("k").stripSuffix("K").toDouble).toLong
      case _ => text.toLong
    }
  }

  /** Converts string with underscores into camel-case strings */
  def camelize(ustring: String): String =
    ustring
      .split("_")
      .map(_.toLowerCase.capitalize)
      .mkString

  /** Split camelcase to separated words */
  def camelizeToWords(string: String,
                      current: List[String] = Nil): List[String] = {
    if (string.nonEmpty) {
      val char = string.tail.find(!_.isLower)
      char match {
        case Some(c) =>
          val index = string.indexOf(c, 1)
          camelizeToWords(string.drop(index),
                          current ::: List(string.take(index)))
        case _ => current ::: List(string)
      }
    } else current
  }

  /** Convert camelcase to underscores */
  def unCamelize(string: String): String = {
    camelizeToWords(string).map(_.toLowerCase).mkString("_")
  }

  /** Function to sort Any values */
  def sortAnyAny(a: Any, b: Any): Boolean = {
    a match {
      case ai: Int =>
        b match {
          case bi: Int    => ai < bi
          case bi: Double => ai < bi
          case _          => a.toString < b.toString
        }
      case _ => a.toString < b.toString
    }
  }

  def loadBiopetProperties(): Unit = {
    val is = getClass.getClassLoader.getResourceAsStream("biopet.properties")
    if (is != null) {
      val prop = System.getProperties
      prop.load(is)
      System.setProperties(prop)
    }
  }
}
