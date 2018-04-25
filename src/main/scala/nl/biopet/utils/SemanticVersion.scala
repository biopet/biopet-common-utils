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
import scala.math.Ordered.orderingToOrdered
import scala.util.matching.Regex

/**
  * Created by pjvanthof on 29/04/2017.
  */
case class SemanticVersion(major: Int,
                           minor: Int,
                           patch: Int,
                           build: Option[String] = None)
    extends Ordered[SemanticVersion] {

  // buildClass is only used for comparison purposes.
  val buildClass: Build = Build(build.getOrElse(""))

  def ==(that: SemanticVersion): Boolean = {
    this.major == that.major &&
    this.minor == that.minor &&
    this.patch == that.patch &&
    this.build == that.build
  }

  def compare(that: SemanticVersion): Int = {
    (this.major, this.minor, this.patch, this.buildClass) compare (that.major, that.minor, that.patch, that.buildClass)
  }
}

object SemanticVersion {
  val semanticVersionRegex: Regex = "[vV]?(\\d+)\\.(\\d+)\\.(\\d+)(-.*)?".r

  /**
    * Check whether a version string is a semantic version.
    *
    * @param version version string
    * @return boolean
    */
  def canParse(version: String): Boolean = fromString(version).isDefined

  /**
    * Check whether a version string is a semantic version.
    * Note: the toInt calls here are only safe because the regex only matches numbers
    *
    * @param version version string
    * @return SemanticVersion case class
    */
  def fromString(version: String): Option[SemanticVersion] = {
    version match {
      case semanticVersionRegex(major, minor, patch, build) =>
        Some(
          SemanticVersion(major.toInt,
                          minor.toInt,
                          patch.toInt,
                          Option(build).map(x => x.stripPrefix("-"))))
      case _ => None
    }
  }

}
