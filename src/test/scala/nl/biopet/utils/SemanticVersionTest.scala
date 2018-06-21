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
import nl.biopet.utils.SemanticVersion._
import org.testng.annotations.Test

/**
  * Created by Sander Bollen on 12-10-16.
  * Here we test [[SemanticVersion]]
  */
class SemanticVersionTest extends BiopetTest {

  val semanticVersion = "1.2.3"
  val semanticVersionWith1 = "v1"
  val semanticVersionWith2 = "V1.2"
  val semanticVersionWith2AndBuild = "v1.2-SNAPSHOT"
  val semanticVersionWithBuild = "1.2.3-alpha0.1"
  val nonSemanticVersion = "v1222.1"
  @Test
  def testIsSemantic(): Unit = {
    canParse(semanticVersion) shouldBe true
    canParse(semanticVersionWithBuild) shouldBe true
    canParse(nonSemanticVersion) shouldBe false
  }

  @Test
  def testMajorVersion(): Unit = {
    fromString(semanticVersion).map(_.major) shouldBe Some(1)
    fromString(semanticVersionWith1).map(_.major) shouldBe Some(1)
    fromString(semanticVersionWith2).map(_.major) shouldBe Some(1)
    fromString(semanticVersionWith2AndBuild).map(_.major) shouldBe Some(1)
    fromString(semanticVersionWithBuild).map(_.major) shouldBe Some(1)
  }

  @Test
  def testMinorVersion(): Unit = {
    fromString(semanticVersion).map(_.minor) shouldBe Some(2)
    fromString(semanticVersionWith1).map(_.minor) shouldBe Some(2)
    fromString(semanticVersionWith2).map(_.minor) shouldBe Some(2)
    fromString(semanticVersionWith2AndBuild).map(_.major) shouldBe Some(2)
    fromString(semanticVersionWithBuild).map(_.minor) shouldBe Some(2)
  }

  @Test
  def testPatchVersion(): Unit = {
    fromString(semanticVersion).map(_.patch) shouldBe Some(3)
    fromString(semanticVersionWith1).map(_.patch) shouldBe Some(3)
    fromString(semanticVersionWith2).map(_.patch) shouldBe Some(3)
    fromString(semanticVersionWith2AndBuild).map(_.major) shouldBe None
    fromString(semanticVersionWithBuild).map(_.patch) shouldBe Some(3)
  }

  @Test
  def testBuildVersion(): Unit = {
    fromString(semanticVersion).flatMap(_.build) shouldBe None
    fromString(semanticVersionWith1).flatMap(_.build) shouldBe None
    fromString(semanticVersionWith2).flatMap(_.build) shouldBe None
    fromString(semanticVersionWith2AndBuild).flatMap(_.build) shouldBe Some("SNAPSHOT")
    fromString(semanticVersionWithBuild).flatMap(_.build) shouldBe Some(
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
  def testMixedDigits(): Unit = {
    fromString("2") > fromString("1.1") shouldBe true
    fromString("1.1") < fromString("1.1.1") shouldBe true
    fromString("1.1-SNAPSHOT") < fromString("1.1") shouldBe true
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
    SemanticVersion(1, 1, 1) < SemanticVersion(1, 1, 1, Some("SNAP")) shouldBe true
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
  @Test
  def testBigVersionComparisons(): Unit = {
    SemanticVersion(1, 1, 1) >= SemanticVersion(1, 200, 2) shouldBe false
    SemanticVersion(3000, 231, 123) >= SemanticVersion(2991, 3231, 432) shouldBe true
  }
  @Test
  def testSort(): Unit = {
    val versions = Seq("v1.0.3", "2.3.3", "0.8.0", "0.8.0-alpha", "0.8.0-beta")
    val sortedVersions = versions.sortBy(version =>
      fromString(version) match {
        case Some(semVer) => semVer
        case _            => new SemanticVersion(0, 0, 0)
    })
    sortedVersions shouldBe Seq("0.8.0-alpha",
                                "0.8.0-beta",
                                "0.8.0",
                                "v1.0.3",
                                "2.3.3")
  }

  def testBigVersionSort(): Unit = {
    // Exceeding Int.MaxValue for the love of it.
    val versions = Seq(
      "v1100.231.41",
      s"${Int.MaxValue + 20}.1.1",
      s"${Int.MaxValue + 10}.123.3-ZZx4",
      s"${Int.MaxValue + 10}.123.3-ZZx5",
      "2.82312123213.31231-XYZbladsa",
      "2.97567567565445.321-beta"
    )
    val sortedVersions = versions.sortBy(version =>
      fromString(version) match {
        case Some(semVer) => semVer
        case _            => new SemanticVersion(0, 0, 0)
    })
    sortedVersions shouldBe Seq(
      "2.82312123213.31231-XYZbladsa",
      "2.97567567565445.321-beta",
      "v1100.231.41",
      s"${Int.MaxValue + 10}.123.3-ZZx4",
      s"${Int.MaxValue + 10}.123.3-ZZx5",
      s"${Int.MaxValue + 20}.1.1"
    )
  }

}
