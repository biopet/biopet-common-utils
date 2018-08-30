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
import org.testng.annotations.{DataProvider, Test}
import play.api.libs.json.Json

import scala.reflect.ClassTag

class DoubleArrayTest extends BiopetTest {
  @DataProvider(name = "validDoubleArrays")
  def validDoubleArrays(): Array[Array[Any]] = Array(
    Array(
      DoubleArray(IndexedSeq("1", "2", "3"), IndexedSeq(1, 2, 3)),
      """{"values":["1","2","3"],"counts":[1,2,3]}""",
      classOf[String]
    ),
    Array(
      DoubleArray(IndexedSeq(1, 2, 3), IndexedSeq(1, 2, 3)),
      """{"values":[1,2,3],"counts":[1,2,3]}""",
      classOf[Int]
    ),
    Array(
      DoubleArray(IndexedSeq(1L, 2L, 3L), IndexedSeq(1, 2, 3)),
      """{"values":[1,2,3],"counts":[1,2,3]}""",
      classOf[Long]
    ),
    Array(
      DoubleArray(IndexedSeq(1.1, 2.2, 3.3), IndexedSeq(1, 2, 3)),
      """{"values":[1.1,2.2,3.3],"counts":[1,2,3]}""",
      classOf[Double]
    ),
    Array(
      DoubleArray(IndexedSeq(12345678910L, 12345678911L, 12345678912L),
                  IndexedSeq(1, 2, 3)),
      """{"values":[12345678910,12345678911,12345678912],"counts":[1,2,3]}""",
      classOf[Long]
    )
  )
  @Test(dataProvider = "validDoubleArrays")
  def testDoubleArrayToJsonSucces(doubleArray: DoubleArray[Any],
                                  jsonString: String,
                                  tag: Class[_]): Unit = {
    Json.stringify(doubleArray.toJson) shouldBe jsonString
  }

  @Test(dataProvider = "validDoubleArrays")
  def testDoubleArrayFromJsonSucces(doubleArray: DoubleArray[Any],
                                    jsonString: String,
                                    tag: Class[_]): Unit = {
    DoubleArray.fromJson(Json.parse(jsonString))(ClassTag(tag)) shouldBe doubleArray
  }

  @Test
  def testDoubleArrayToJsonFail(): Unit = {
    intercept[Exception] {
      val test = DoubleArray.fromJson[Int](Json.parse(
        """{"values":["bla","Heyo","Take me to your leader"],"counts":[1,2,3]}"""))
    }
  }

  @Test
  def testDoubleArrayNotEqualLengthsFail(): Unit = {
    intercept[IllegalArgumentException] {
      DoubleArray(IndexedSeq(2, 3), IndexedSeq(1, 2, 3))
    }.getMessage shouldBe "requirement failed: Values and counts do not have the same length."
  }

  def testDoubleArrayNonUniqueValues(): Unit = {
    intercept[IllegalArgumentException] {
      DoubleArray[Int](IndexedSeq(1, 1), IndexedSeq(1, 2))
    }.getMessage shouldBe
      "requirement failed: Non-unique values detected. " +
        "Values map to dictionary keys, and each key should be unique."
  }

  @Test
  def testDoubleArrayFromJsonFail(): Unit = {
    val doubleArray = DoubleArray(IndexedSeq(2, 3L, "bla"), IndexedSeq(0, 1, 2))
    val jsonString = """{"values":[2,3,"bla"],"counts":[0,1,2]}"""

    intercept[IllegalStateException] {
      DoubleArray.fromJson[Int](Json.parse(jsonString)) shouldBe doubleArray
    }.getMessage should include("JsError")
  }

  @Test
  def testDoubleArrayEquals(): Unit = {
    // Order should not matter. 3->3, 2->2 , 1->1 so these are equal.
    DoubleArray(IndexedSeq(1, 2, 3), IndexedSeq(1, 2, 3)) == DoubleArray(
      IndexedSeq(3, 2, 1),
      IndexedSeq(3, 2, 1)) shouldBe true
  }
}
