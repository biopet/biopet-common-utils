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
                           minor: Option[Int] = None,
                           patch: Option[Int] = None,
                           build: Option[String] = None)
    extends Ordered[SemanticVersion] {

  def ==(that: SemanticVersion): Boolean = {
    this.major == that.major &&
    this.minor == that.minor &&
    this.patch == that.patch &&
    this.build == that.build
  }

  /**
    * Checks whether one version is later than another.
    * Versions without builds (no -alpha, -SNAPSHOT or -build123)
    * are assumed to be later than versions with builds.
    * Example 0.8.0-alpha < 0.8.0-beta < 0.8.0
    * @param that
    * @return
    */
  def compare(that: SemanticVersion): Int = {
    val versionCompare = (this.major, this.minor, this.patch) compare (that.major, that.minor, that.patch)
    if (versionCompare == 0) {
      // Empty builds should always be greatest.
      // Example 0.8.0-alpha < 0.8.0 and 1.0.2-SNAPSHOT < 1.0.2
      (this.build, that.build) match {
        case (Some(_), None)    => -1
        case (None, Some(_))    => +1
        case (Some(a), Some(b)) => a compare b
        case _                  => 0
      }
    } else versionCompare
  }
}

object SemanticVersion {
  val semanticVersionRegex: Regex = "[vV]?(\\d+)\\.?(\\d+)\\.?(\\d+)(-.*)?".r

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
                          Some(minor.toInt),
                          Some(patch.toInt),
                          Option(build).map(x => x.stripPrefix("-"))))
      case _ => None
    }
  }
}
