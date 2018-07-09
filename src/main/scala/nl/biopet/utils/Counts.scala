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

import java.io.{File, PrintWriter}

import nl.biopet.utils.conversions.anyToJson
import play.api.libs.json._
import nl.biopet.utils.Counts.Implicits._
import scala.collection.mutable

/**
  * Created by pjvanthof on 05/07/16.
  */
class Counts[T](c: Map[T, Long] = Map[T, Long]())(implicit ord: Ordering[T])
    extends Serializable {
  protected[Counts] val counts: mutable.Map[T, Long] = mutable.Map() ++ c

  /** Returns histogram as map */
  def countsMap: Map[T, Long] = counts.toMap

  /** Returns value if it does exist */
  def get(key: T): Option[Long] = counts.get(key)

  /** This will add an other histogram to `this` */
  def +=(other: Counts[T]): this.type = {
    other.counts.foreach {
      case (k, v) =>
        this.counts += k -> (this.counts.getOrElse(k, 0L) + v)
    }
    this
  }

  /** With this a value can be added to the histogram */
  def add(value: T): Unit = {
    counts += value -> (counts.getOrElse(value, 0L) + 1)
  }

  /** With this multiple values of the same content can be added to the histogram */
  def addMulti(value: T, number: Long): Unit = {
    counts += value -> (counts.getOrElse(value, 0L) + number)
  }

  /** Write histogram to a tsv/count file */
  def writeHistogramToTsv(file: File): Unit = {
    val writer = new PrintWriter(file)
    writer.println("value\tcount")
    counts.keys.toList.sorted.foreach(x => writer.println(s"$x\t${counts(x)}"))
    writer.close()
  }

  def toSummaryMap: Map[String, List[Any]] = {
    val values = counts.keySet.toList.sortWith(sortAnyAny)
    Map("values" -> values, "counts" -> values.map(counts(_)))
  }

  override def equals(other: Any): Boolean = {
    other match {
      case c: Counts[T] => this.counts == c.counts
      case _            => false
    }
  }

  /** Returns acumolated counts */
  def acumolateCounts(reverse: Boolean = false): Map[T, Long] = {
    val map = countsMap
    val keys = map.keys.toList.sorted
    var total = 0L
    (for (key <- if (reverse) keys.reverse else keys) yield {
      if (map(key) != 0L) {
        total += map(key)
        Some(key -> total)
      } else None
    }).flatten.toMap
  }

  /** Returns total number of observations */
  def total: Long = {
    counts.values.sum
  }

  def toJson: JsValue = {
    conversions.mapToJson(counts.map { case (k, v) => k.toString -> v }.toMap)
  }

  /**
    * Converts to two IndexedSeqs, one containing values, and one containing counts.
    * @return Returns a doubleArray
    */
  def toDoubleArray: Counts.DoubleArray[T] = {
    val (keySeq, countSeq) =
      counts.foldLeft((IndexedSeq[T](), IndexedSeq[Long]())) {
        case ((keyList, countList), (key, count)) =>
          (keyList :+ key, countList :+ count)
      }
    Counts.DoubleArray(keySeq, countSeq)
  }

}

object Counts {

  /** This will write multiple counts into a single file */
  def writeMultipleCounts[T](
      countMap: Map[String, Counts[T]],
      outputFile: File,
      headerPrefix: String = "Sample",
      acumolate: Boolean = false,
      reverse: Boolean = false)(implicit ord: Ordering[T]): Unit = {
    val writer = new PrintWriter(outputFile)
    writer.println(countMap.keys.mkString(s"$headerPrefix\t", "\t", ""))
    val keys =
      countMap
        .foldLeft(Set[T]()) { case (a, (_, b)) => a ++ b.counts.keys }
        .toList
        .sorted
    val counts = if (acumolate) {
      countMap.map { case (k, v) => k -> v.acumolateCounts(reverse) }
    } else countMap.map { case (k, v) => k -> v.countsMap }
    for (value <- if (reverse) keys.reverse else keys) {
      writer.println(
        countMap
          .map { case (k, _) => counts(k).getOrElse(value, "") }
          .mkString(value + "\t", "\t", ""))
    }
    writer.close()
  }

  def fromDoubleArray[T](doubleArray: Counts.DoubleArray[T])(
      implicit ord: Ordering[T]): Counts[T] = {
    new Counts[T](doubleArray.toMap)
  }

  private case class Schema(map: Map[String, Long])

  object Implicits {
    implicit def indexedSeqWrites[T]: Writes[IndexedSeq[T]] =
      new Writes[IndexedSeq[T]] {
        def writes(indexedSeq: IndexedSeq[T]): JsValue =
          anyToJson(indexedSeq.toList.flatMap(x => {
            val json = anyToJson(x)
            json match {
              case JsNull =>
                throw new IllegalStateException("List element may not be null")
              case _ => Some(json)
            }
          }))
      }
    implicit def readsAny[T]: Reads[T] =
      new Reads[T] {
        def reads(json: JsValue): JsResult[T] =
          List(json.validate[Int],
               json.validate[Float],
               json.validate[Double],
               json.validate[Long],
               json.validate[String])
            .filter(_.isSuccess)
            .headOption
            .getOrElse(throw new IllegalStateException(""))
            .map(
              _ match {
                case a: T => a
              }
            )
      }

    implicit def doubleArrayReads[T]: Reads[Counts.DoubleArray[T]] =
      Json.reads[Counts.DoubleArray[T]]

    implicit def doubleArrayWrites[T]: Writes[Counts.DoubleArray[T]] =
      Json.writes[Counts.DoubleArray[T]]

  }

  /**
    * A class that stores a T,Long dictionary as two sequences, that can be zipped.
    * @param values An IndexedSeq of values
    * @param counts An IndexedSeq of counts
    * @tparam T A jsonifiable type
    */
  case class DoubleArray[T](values: IndexedSeq[T], counts: IndexedSeq[Long]) {
    require(values.size == counts.size,
            "Values and counts do not have the same length.")

    def toMap: Map[T, Long] = this.values.zip(this.counts).toMap

    def toJson: JsValue = Json.toJson(this)
  }

  object DoubleArray {
    def fromJson[T](json: JsValue): DoubleArray[T] = ???
  }

  def mapFromJson(json: JsValue): Map[String, Long] = {
    implicit val read: Reads[Schema] = Json.reads[Schema]
    Json.reads[Schema].reads(json) match {
      case x: JsSuccess[Schema] => x.value.map
      case e: JsError           => throw new IllegalStateException(e.toString)
    }
  }
}
