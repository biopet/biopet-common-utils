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
import org.scalatest.Matchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test
import nl.biopet.utils.SemanticVersion._

/**
  * Created by Sander Bollen on 12-10-16.
  * Here we test [[SemanticVersion]]
  */
class SemanticVersionTest extends BiopetTest {

  val semanticVersion = "1.2.3"
  val semanticVersionWith_v = "v1.2.3"
  val semanticVersionWith_V = "V1.2.3"
  val semanticVersionWithBuild = "1.2.3-alpha0.1"
  val nonSemanticVersion = "v1222.1"

  @Test
  def testIsSemantic(): Unit = {
    isSemanticVersion(semanticVersion) shouldBe true
    isSemanticVersion(semanticVersionWithBuild) shouldBe true
    isSemanticVersion(nonSemanticVersion) shouldBe false
  }

  @Test
  def testMajorVersion(): Unit = {
    getSemanticVersion(semanticVersion).map(_.major) shouldBe Some(1)
    getSemanticVersion(semanticVersionWith_v).map(_.major) shouldBe Some(1)
    getSemanticVersion(semanticVersionWith_V).map(_.major) shouldBe Some(1)
    getSemanticVersion(semanticVersionWithBuild).map(_.major) shouldBe Some(1)
  }

  @Test
  def testMinorVersion(): Unit = {
    getSemanticVersion(semanticVersion).map(_.minor) shouldBe Some(2)
    getSemanticVersion(semanticVersionWith_v).map(_.minor) shouldBe Some(2)
    getSemanticVersion(semanticVersionWith_V).map(_.minor) shouldBe Some(2)
    getSemanticVersion(semanticVersionWithBuild).map(_.minor) shouldBe Some(2)
  }

  @Test
  def testPatchVersion(): Unit = {
    getSemanticVersion(semanticVersion).map(_.patch) shouldBe Some(3)
    getSemanticVersion(semanticVersionWith_v).map(_.patch) shouldBe Some(3)
    getSemanticVersion(semanticVersionWith_V).map(_.patch) shouldBe Some(3)
    getSemanticVersion(semanticVersionWithBuild).map(_.patch) shouldBe Some(3)
  }

  @Test
  def testBuildVersion(): Unit = {
    getSemanticVersion(semanticVersion).flatMap(_.build) shouldBe None
    getSemanticVersion(semanticVersionWith_v).flatMap(_.build) shouldBe None
    getSemanticVersion(semanticVersionWith_V).flatMap(_.build) shouldBe None
    getSemanticVersion(semanticVersionWithBuild).flatMap(_.build) shouldBe Some(
      "alpha0.1")
  }

  @Test
  def testGreaterThen(): Unit = {
    SemanticVersion(1, 1, 1) > SemanticVersion(1, 1, 1) shouldBe false
    SemanticVersion(1, 1, 1) > SemanticVersion(0, 1, 1) shouldBe true
    SemanticVersion(1, 1, 1) > SemanticVersion(1, 0, 1) shouldBe true
    SemanticVersion(1, 1, 1) > SemanticVersion(1, 1, 0) shouldBe true
    SemanticVersion(1, 1, 1) > SemanticVersion(2, 1, 1) shouldBe false
    SemanticVersion(1, 1, 1) > SemanticVersion(1, 2, 1) shouldBe false
    SemanticVersion(1, 1, 1) > SemanticVersion(1, 1, 2) shouldBe false
  }

  @Test
  def testLesserThen(): Unit = {
    SemanticVersion(1, 1, 1) < SemanticVersion(1, 1, 1) shouldBe false
    SemanticVersion(1, 1, 1) < SemanticVersion(0, 1, 1) shouldBe false
    SemanticVersion(1, 1, 1) < SemanticVersion(1, 0, 1) shouldBe false
    SemanticVersion(1, 1, 1) < SemanticVersion(1, 1, 0) shouldBe false
    SemanticVersion(1, 1, 1) < SemanticVersion(2, 1, 1) shouldBe true
    SemanticVersion(1, 1, 1) < SemanticVersion(1, 2, 1) shouldBe true
    SemanticVersion(1, 1, 1) < SemanticVersion(1, 1, 2) shouldBe true
  }

  @Test
  def testGreaterThenOrEqual(): Unit = {
    SemanticVersion(1, 1, 1) >= SemanticVersion(1, 1, 1) shouldBe true
    SemanticVersion(1, 1, 1) >= SemanticVersion(0, 1, 1) shouldBe true
    SemanticVersion(1, 1, 1) >= SemanticVersion(1, 0, 1) shouldBe true
    SemanticVersion(1, 1, 1) >= SemanticVersion(1, 1, 0) shouldBe true
    SemanticVersion(1, 1, 1) >= SemanticVersion(2, 1, 1) shouldBe false
    SemanticVersion(1, 1, 1) >= SemanticVersion(1, 2, 1) shouldBe false
    SemanticVersion(1, 1, 1) >= SemanticVersion(1, 1, 2) shouldBe false
  }

  @Test
  def testLesserThenOrEqual(): Unit = {
    SemanticVersion(1, 1, 1) <= SemanticVersion(1, 1, 1) shouldBe true
    SemanticVersion(1, 1, 1) <= SemanticVersion(0, 1, 1) shouldBe false
    SemanticVersion(1, 1, 1) <= SemanticVersion(1, 0, 1) shouldBe false
    SemanticVersion(1, 1, 1) <= SemanticVersion(1, 1, 0) shouldBe false
    SemanticVersion(1, 1, 1) <= SemanticVersion(2, 1, 1) shouldBe true
    SemanticVersion(1, 1, 1) <= SemanticVersion(1, 2, 1) shouldBe true
    SemanticVersion(1, 1, 1) <= SemanticVersion(1, 1, 2) shouldBe true
  }

}
