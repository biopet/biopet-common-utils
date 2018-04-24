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
import scala.util.matching.Regex
import scala.math.pow

/**
  * Created by pjvanthof on 29/04/2017.
  */
case class SemanticVersion(major: Int,
                           minor: Int,
                           patch: Int,
                           build: Option[String] = None)
    extends Ordered[SemanticVersion] {

  def ==(that: SemanticVersion): Boolean = {
    this.major == that.major &&
    this.minor == that.minor &&
    this.patch == that.patch &&
    this.build == that.build
  }
  override def >(that: SemanticVersion): Boolean = {
    if (this.major != that.major) this.major > that.major
    else if (this.minor != that.minor) this.minor > that.minor
    else this.patch > that.patch
  }

  override def <(that: SemanticVersion): Boolean = {
    if (this.major != that.major) this.major < that.major
    else if (this.minor != that.minor) this.minor < that.minor
    else this.patch < that.patch
  }

  override def >=(that: SemanticVersion): Boolean = {
    this == that || this > that
  }

  override def <=(that: SemanticVersion): Boolean = {
    this == that || this < that
  }

  /**
    * Converts the build to an Int for comparison purposes
    * @return an integer
    */
  def buildToInt(): Int = {
    // TODO: Improve algorithm for buildToInt
    build match {
      case Some(string) =>
        // Currently only sorting on the first character of the build.
        string.toCharArray.headOption match {
          // The values are arbitrary. I have range -999 to 999 available for comparing the build.
          // If a build is attached it should rank lower than the release. Therefore penalize.
          // 100-char.asDigit. If build starts with "b" (11) this number will be higher than
          // if it starts with a (10). Beta is higher than alpha.
          case Some(char) =>
            if (char.asDigit < 0) -150 else -(100 - char.asDigit)
          // If empty => 0. If no build is attached this is the release.
          case _ => 0
        }
      case _ => 0
    }
  }

  /**
    * Converts this version to an integer for comparison purposes
    * Does not work properly with major, minor, or patch versions > 100
    * @return the build to int.
    */
  def toInt(): Int = {
    // Int.MaxValue == 2147483647
    require(
      major < 214,
      s"With a major value higher than 213 this class cannot be sorted. Major: $major")
    require(
      minor < 100,
      s"With a minor value higher than 99 this class cannot be sorted. Minor: $minor")
    require(
      patch < 100,
      s"With a patch value higher than 99 this class cannot be sorted. Patch: $patch")
    this.major * pow(10, 7) +
      this.minor * pow(10, 5) +
      this.patch * pow(10, 3) +
      buildToInt()
  }.toInt

  def compare(that: SemanticVersion): Int = {
    this.toInt() - that.toInt()
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
