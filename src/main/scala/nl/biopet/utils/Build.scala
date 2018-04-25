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

/**
  * A class that is a string, but orders differently
  * Empty builds are largest instead of smallest
  * @param build The build as a string.
  */
case class Build(build: String) extends Ordered[Build] {
  override def toString: String = this.build

  def compare(that: Build): Int = {
    // Empty builds should always be greatest.
    // Example 0.8.0-alpha < 0.8.0 and 1.0.2-SNAPSHOT < 1.0.2
    (this.build.isEmpty, that.build.isEmpty) match {
      case (true, false) => Int.MaxValue
      case (false, true) => Int.MinValue
      case _ => this.build compare that.build
    }
  }

  def ==(that: Build): Boolean = {
    this.build == that.build
  }
}
