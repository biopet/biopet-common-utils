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
import nl.biopet.utils.Counts.DoubleArray
import org.testng.annotations.Test
import play.api.libs.json.Json

import scala.io.Source

/**
  * Created by pjvan_thof on 19-7-16.
  */
class CountsTest extends BiopetTest {
  @Test
  def testValues(): Unit = {
    val data: Map[String, Long] = Map("1" -> 1, "2" -> 2, "3" -> 3)
    val c1 = new Counts[String](data)
    c1.countsMap shouldBe data
    c1.get("1") shouldBe Some(1)
    c1.get("2") shouldBe Some(2)
    c1.get("3") shouldBe Some(3)
    c1.get("4") shouldBe None

    c1.add("1")
    c1.get("1") shouldBe Some(2)
    c1.add("4")
    c1.get("4") shouldBe Some(1)

    val c2 = new Counts[String](data)
    c1 += c2

    c1.get("1") shouldBe Some(3)
    c1.get("2") shouldBe Some(4)
    c1.get("3") shouldBe Some(6)
    c1.get("4") shouldBe Some(1)
  }

  @Test
  def testEmpty(): Unit = {
    val c1 = new Counts[Int]()
    c1.countsMap.isEmpty shouldBe true
  }

  @Test
  def testEqual(): Unit = {
    val c1 = new Counts[Int]()
    val c2 = new Counts[Int]()

    c1 should not be "be a string"

    c1 shouldBe c1
    c2 shouldBe c2
    c1 shouldBe c2

    c1.add(1)
    c1 shouldBe c1
    c2 shouldBe c2
    c1 should not be c2

    c2.add(1)
    c1 shouldBe c1
    c2 shouldBe c2
    c1 shouldBe c2
  }

  @Test
  def testTotal(): Unit = {
    val c1 = new Counts[Int]()
    c1.total shouldBe 0L
    c1.add(4)
    c1.total shouldBe 1L
    c1.addMulti(2, 4)
    c1.total shouldBe 5L
  }

  @Test
  def testAddMulti(): Unit = {
    val c1 = new Counts[Int]()
    c1.add(4)
    c1.get(4) shouldBe Some(1L)

    c1.addMulti(4, 5)
    c1.get(4) shouldBe Some(6L)

    c1.get(5) shouldBe None
    c1.addMulti(5, 0)
    c1.get(5) shouldBe Some(0L)
  }

  @Test
  def testTsv(): Unit = {
    val data: Map[Int, Long] = Map(1 -> 1, 2 -> 2, 3 -> 3)
    val c1 = new Counts[Int](data)

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
  def testWriteMultiCounts(): Unit = {
    val c1 = new Counts[Int](Map(4 -> 4, 3 -> 1))
    val c2 = new Counts[Int](Map(5 -> 1, 3 -> 2))
    val outputFile = File.createTempFile("test.", ".tsv")
    outputFile.deleteOnExit()
    Counts.writeMultipleCounts(Map("c1" -> c1, "c2" -> c2), outputFile)
    val reader = Source.fromFile(outputFile)
    val it = reader.getLines()
    it next () shouldBe "Sample\tc1\tc2"
    it.next shouldBe "3\t1\t2"
    it.next shouldBe "4\t4\t"
    it.next shouldBe "5\t\t1"
    it.hasNext shouldBe false
    reader.close()
  }

  @Test
  def testWriteMultiCountsReverse(): Unit = {
    val c1 = new Counts[Int](Map(4 -> 4, 3 -> 1))
    val c2 = new Counts[Int](Map(5 -> 1, 3 -> 2))
    val outputFile = File.createTempFile("test.", ".tsv")
    outputFile.deleteOnExit()
    Counts.writeMultipleCounts(Map("c1" -> c1, "c2" -> c2),
                               outputFile,
                               reverse = true)
    val reader = Source.fromFile(outputFile)
    val it = reader.getLines()
    it next () shouldBe "Sample\tc1\tc2"
    it.next shouldBe "5\t\t1"
    it.next shouldBe "4\t4\t"
    it.next shouldBe "3\t1\t2"
    it.hasNext shouldBe false
    reader.close()
  }

  @Test
  def testWriteMultiCountsAcumolate(): Unit = {
    val c1 = new Counts[Int](Map(4 -> 4, 3 -> 1))
    val c2 = new Counts[Int](Map(5 -> 1, 3 -> 2))
    val outputFile = File.createTempFile("test.", ".tsv")
    outputFile.deleteOnExit()
    Counts.writeMultipleCounts(Map("c1" -> c1, "c2" -> c2),
                               outputFile,
                               acumolate = true)
    val reader = Source.fromFile(outputFile)
    val it = reader.getLines()
    it next () shouldBe "Sample\tc1\tc2"
    it.next shouldBe "3\t1\t2"
    it.next shouldBe "4\t5\t"
    it.next shouldBe "5\t\t3"
    it.hasNext shouldBe false
    reader.close()
  }

  @Test
  def testAcumolateCounts(): Unit = {
    val c1 = new Counts[Int](Map(1 -> 1, 2 -> 2, 3 -> 3))
    c1.acumolateCounts() shouldBe Map(1 -> 1, 2 -> 3, 3 -> 6)
    c1.acumolateCounts(true) shouldBe Map(1 -> 6, 2 -> 5, 3 -> 3)
  }

  @Test
  def testToDoubleArray(): Unit = {
    val c1 = new Counts[String](Map("1" -> 1, "2" -> 2, "3" -> 3))
    val doubleArray = c1.toDoubleArray
    doubleArray.values.toSet shouldBe Set("1", "2", "3")
    doubleArray.counts.toSet shouldBe Set(1, 2, 3)
    Counts.fromDoubleArray(doubleArray) shouldBe c1
  }

  @Test
  def testDoubleArrayToJsonSucces(): Unit = {
    val daString = DoubleArray(IndexedSeq("1", "2", "3"), IndexedSeq(1, 2, 3))
    Json.stringify(daString.toJson) shouldBe
      """{"values":["1","2","3"],"counts":[1,2,3]}"""

    val daInt = DoubleArray(IndexedSeq(1, 2, 3), IndexedSeq(1, 2, 3))
    Json.stringify(daInt.toJson) shouldBe
      """{"values":[1,2,3],"counts":[1,2,3]}"""

    val daLong = DoubleArray(IndexedSeq(1L, 2L, 3L), IndexedSeq(1, 2, 3))
    Json.stringify(daLong.toJson) shouldBe
      """{"values":[1,2,3],"counts":[1,2,3]}"""

    val daFloat = DoubleArray(IndexedSeq(1.1, 2.2, 3.3), IndexedSeq(1, 2, 3))
    Json.stringify(daFloat.toJson) shouldBe
      """{"values":[1.1,2.2,3.3],"counts":[1,2,3]}"""

    val daDouble =
      DoubleArray(IndexedSeq(1.1D, 2.2D, 3.3D), IndexedSeq(1, 2, 3))
    Json.stringify(daDouble.toJson) shouldBe
      """{"values":[1.1,2.2,3.3],"counts":[1,2,3]}"""
  }

  @Test
  def testDoubleArrayToJsonFail(): Unit = ???

  @Test
  def testDoubleArrayFromJsonSucces(): Unit = ???

  @Test
  def testDoubleArrayFromJsonFail(): Unit = ???
}
