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

import nl.biopet.utils.Counts.Schema
import nl.biopet.utils.conversions.anyToJson
import nl.biopet.utils.DoubleArray.Implicits._
import play.api.libs.json._
import scala.reflect.runtime.universe.TypeTag
/**
  * A class that stores a T,Long dictionary as two sequences, that can be zipped.
  *
  * @param values An IndexedSeq of values
  * @param counts An IndexedSeq of counts
  * @tparam T A jsonifiable type
  */
case class DoubleArray[T](values: IndexedSeq[T], counts: IndexedSeq[Long]) {
  require(values.size == counts.size,
          "Values and counts do not have the same length.")
  require(values.distinct == values, "Non-unique values detected. Values map to dictionary keys, and each key should be unique.")

  def toMap: Map[T, Long] = this.values.zip(this.counts).toMap

  def toJson: JsValue = Json.toJson(this)
}
object DoubleArray {

  /**
    * Convert a JsValue into a DoubleArray
    * @param json a JsValue
    * @tparam T The type of the items in the value list.
    * @return a DoubleArray
    */
  def fromJson[T](json: JsValue)(implicit tag: TypeTag[T]): DoubleArray[T] = {
    implicit def read: Reads[DoubleArray[T]] = Json.reads[DoubleArray[T]]
    Json.reads[DoubleArray[T]].reads(json) match {
      case x: JsSuccess[DoubleArray[T]] => x.value
      case e: JsError                   => throw new IllegalStateException(e.toString)
    }
  }

  /**
    * Create a map[String,Long] using the schema
    * @param json the jsvalue
    * @return a map.
    */
  def mapFromJson(json: JsValue): Map[String, Long] = {
    implicit val read: Reads[Schema] = Json.reads[Schema]
    Json.reads[Schema].reads(json) match {
      case x: JsSuccess[Schema] => x.value.map
      case e: JsError           => throw new IllegalStateException(e.toString)
    }
  }
  object Implicits {

    /**
      * Method to write an indexedSeq to json.
      *
      * @tparam T IndexedSeq can be of any type.
      * @return A Writes object
      */
    implicit def indexedSeqWrites[T]: Writes[IndexedSeq[T]] =
      new Writes[IndexedSeq[T]] {
        def writes(indexedSeq: IndexedSeq[T]): JsValue =
          anyToJson(indexedSeq.toList.map(anyToJson))
      }

    /**
      * Method to read an indexedSeq from json.
      * @tparam T Can be of type int,long,double or string
      * @return A writhe method.
      */
    implicit def indexedSeqReads[T](implicit tag: TypeTag[T]): Reads[IndexedSeq[T]] = {
      new Reads[IndexedSeq[T]] {
        def reads(json: JsValue): JsResult[IndexedSeq[T]] = {
          // First evaluate if it can be parsed as an index holding a simple type
          List(json.validate[List[Int]],
               json.validate[List[Long]],
               json.validate[List[Double]],
               json.validate[List[String]])
            .flatMap {
              case JsSuccess(value: List[T], path) =>
                Some(JsSuccess(value.toIndexedSeq, path))
              case _ => None
            }
            .headOption
            .getOrElse(JsError())
        }
      }
    }

    /**
      * Read double array type.
      * @tparam T The type of the items in the values list.
      * @return a reads method.
      */
    implicit def doubleArrayReads[T](implicit tag: TypeTag[T]): Reads[DoubleArray[T]] =
      new Reads[DoubleArray[T]] {
        def reads(json: JsValue): JsResult[DoubleArray[T]] = {
          json match {
            case o: JsObject =>
              val values: JsResult[IndexedSeq[T]] =
                o.value("values").validate[IndexedSeq[T]]
              val counts: JsResult[IndexedSeq[Long]] =
                o.value("counts").validate[IndexedSeq[Long]]
              JsSuccess(
                DoubleArray(
                  values.getOrElse(throw new IllegalStateException(
                    "Values could not be parsed from json file.")),
                  counts.getOrElse(throw new IllegalStateException(
                    "Counts could not be parsed from json file."))
                ))

            case _ => throw new IllegalStateException("Not a object")
          }
        }
      }

    /**
      * A method to write doublearrays
      * @tparam T the type of the items in the value list.
      * @return A json writes method.
      */
    implicit def doubleArrayWrites[T]: Writes[DoubleArray[T]] =
      Json.writes[DoubleArray[T]]

  }
}
