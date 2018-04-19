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

import java.io.File

import nl.biopet.test.BiopetTest
import org.scalatest.Matchers
import org.scalatest.testng.TestNGSuite
import org.testng.annotations.Test

import scala.io.Source

/**
  * Created by pjvan_thof on 19-7-16.
  */
class HistogramTest extends BiopetTest {
  @Test
  def testValues(): Unit = {
    val data: Map[Int, Long] = Map(1 -> 1, 2 -> 2, 3 -> 3)
    val c1 = new Histogram[Int](data)
    c1.countsMap shouldBe data
    c1.get(1) shouldBe Some(1)
    c1.get(2) shouldBe Some(2)
    c1.get(3) shouldBe Some(3)
    c1.get(4) shouldBe None

    c1.add(1)
    c1.get(1) shouldBe Some(2)
    c1.add(4)
    c1.get(4) shouldBe Some(1)

    val c2 = new Counts[Int](data)
    c1 += c2

    c1.get(1) shouldBe Some(3)
    c1.get(2) shouldBe Some(4)
    c1.get(3) shouldBe Some(6)
    c1.get(4) shouldBe Some(1)
  }

  @Test
  def testEmpty(): Unit = {
    val c1 = new Histogram[Int]()
    c1.countsMap.isEmpty shouldBe true
  }

  @Test
  def testTsv(): Unit = {
    val data: Map[Int, Long] = Map(1 -> 1, 2 -> 2, 3 -> 3)
    val c1 = new Histogram[Int](data)

    val tsvFile = File.createTempFile("counts.", ".tsv")
    tsvFile.deleteOnExit()

    c1.writeHistogramToTsv(tsvFile)

    val reader = Source.fromFile(tsvFile)
    reader.getLines().toList shouldBe List("value\tcount",
                                           "1\t1",
                                           "2\t2",
                                           "3\t3")
    reader.close()
  }

  @Test
  def testAggregateStats(): Unit = {
    val data: Map[Int, Long] = Map(1 -> 1, 2 -> 2, 3 -> 3)
    val c1 = new Histogram[Int](data)
    c1.aggregateStats shouldBe Map("modal" -> 3,
                                   "mean" -> 2.3333333333333335,
                                   "min" -> 1,
                                   "max" -> 3,
                                   "median" -> 1)
  }

  @Test
  def testAggregateStatsFile(): Unit = {
    val data: Map[Int, Long] = Map(1 -> 1, 2 -> 2, 3 -> 3)
    val c1 = new Histogram[Int](data)
    val outputFile = File.createTempFile("test.", ".txt")
    outputFile.deleteOnExit()
    c1.writeAggregateToTsv(outputFile)

    Source.fromFile(outputFile).getLines().toList shouldBe List(
      "modal\t3",
      "mean\t2.3333333333333335",
      "min\t1",
      "max\t3",
      "median\t1"
    )
  }

  @Test
  def testRead(): Unit = {
    val c1 = new Histogram[Int](Map(1 -> 1, 2 -> 2, 3 -> 3))
    val outputFile = File.createTempFile("test.", ".tsv")
    outputFile.deleteOnExit()
    Counts.writeMultipleCounts(Map("c1" -> c1), outputFile)

    Histogram.fromFile(outputFile, _.toInt)

    val histograms = Histogram.fromMultiHistogramFile(outputFile, _.toInt)

    histograms("c1").countsMap shouldBe Map(1 -> 1, 2 -> 2, 3 -> 3)
    histograms.get("c2") shouldBe None
  }

  @Test
  def testReadMulti(): Unit = {
    val c1 = new Histogram[Int](Map(1 -> 1, 2 -> 2, 3 -> 3))
    val c2 = new Histogram[Int](Map(1 -> 2, 2 -> 1, 4 -> 2))
    val outputFile = File.createTempFile("test.", ".tsv")
    outputFile.deleteOnExit()
    Counts.writeMultipleCounts(Map("c1" -> c1, "c2" -> c2), outputFile)

    intercept[IllegalArgumentException] {
      Histogram.fromFile(outputFile, _.toInt)
    }.getMessage shouldBe s"requirement failed: File has multiple histograms: $outputFile"

    val histograms = Histogram.fromMultiHistogramFile(outputFile, _.toInt)

    histograms("c1").countsMap shouldBe Map(1 -> 1, 2 -> 2, 3 -> 3)
    histograms("c2").countsMap shouldBe Map(1 -> 2, 2 -> 1, 4 -> 2)
  }
}
