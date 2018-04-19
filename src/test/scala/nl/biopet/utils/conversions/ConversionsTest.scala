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

package nl.biopet.utils.conversions

import java.io.{File, PrintWriter}
import java.util

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test
import play.api.libs.json.{
  JsArray,
  JsBoolean,
  JsNull,
  JsNumber,
  JsObject,
  JsString
}

import scala.collection.immutable.ListMap
import scala.io.Source

class ConversionsTest extends BiopetTest {
  @Test
  def testAnyToList(): Unit = {
    val input1: Any = List(1, 2)
    anyToList(input1) shouldBe input1
    anyToList(Some(input1)) shouldBe input1

    val input2: Any = List("1", 2)
    anyToList(input2) shouldBe input2

    val input3: Any = "test"
    anyToList(input3) shouldBe input3 :: Nil

    val input4: util.ArrayList[String] = new util.ArrayList()
    input4.add("1")
    input4.add("2")
    anyToList(input4) shouldBe List("1", "2")

    anyToList(None) shouldBe Nil
    anyToList(null) shouldBe Nil
  }

  @Test
  def testAnyToStringList(): Unit = {
    anyToStringList(List("1", "2")) shouldBe List("1", "2")
    anyToStringList(List(1, 2)) shouldBe List("1", "2")
  }

  @Test
  def testAnyToDoubleList(): Unit = {
    anyToDoubleList(List("1", "2")) shouldBe List(1.0, 2.0)
    anyToDoubleList(List(1, 2)) shouldBe List(1.0, 2.0)
    anyToDoubleList(List(1.0, 2.0)) shouldBe List(1.0, 2.0)
  }

  @Test
  def testReadYaml(): Unit = {
    val outputFile = File.createTempFile("test.", ".yml")
    val writer = new PrintWriter(outputFile)
    writer.println("test: test")
    writer.println("test2: 2")
    writer.close()
    yamlFileToMap(outputFile) shouldBe Map("test" -> "test", "test2" -> 2)
  }

  @Test
  def testEmptyReadYaml(): Unit = {
    val outputFile = File.createTempFile("test.", ".yml")
    intercept[IllegalStateException] {
      yamlFileToMap(outputFile)
    }
  }

  @Test
  def testMergeMaps(): Unit = {
    mergeMaps(Map(), Map()) shouldBe Map()
    mergeMaps(Map("key1" -> 1), Map("key2" -> 2)) shouldBe Map("key1" -> 1,
                                                               "key2" -> 2)

    mergeMaps(Map("key1" -> 1), Map("key1" -> 2)) shouldBe Map("key1" -> 1)

    intercept[IllegalArgumentException] {
      mergeMaps(Map("key1" -> 1),
                Map("key1" -> 2),
                (_, _, _) => throw new IllegalArgumentException) shouldBe Map(
        "key1" -> 1)
    }

    mergeMaps(Map("map" -> Map("key1" -> 1)), Map("map" -> Map("key2" -> 2))) shouldBe Map(
      "map" -> Map("key1" -> 1, "key2" -> 2))
    mergeMaps(Map("map" -> Map("key1" -> 1)), Map("map" -> "something else")) shouldBe Map(
      "map" -> Map("key1" -> 1))
  }

  @Test
  def testAnyToMap(): Unit = {
    any2map(null) shouldBe null
    any2map(Map("bla" -> 3)) shouldBe Map("bla" -> 3)
    any2map(new java.util.LinkedHashMap) shouldBe Map()

    intercept[IllegalStateException] {
      any2map("not_a_map")
    }.getMessage shouldBe "Value 'not_a_map' is not an Map"
  }

  @Test
  def testAnyToJson(): Unit = {
    anyToJson(null) shouldBe JsNull
    anyToJson(None) shouldBe JsNull
    anyToJson(4) shouldBe JsNumber(4)
    anyToJson(4.0) shouldBe JsNumber(4.0)
    anyToJson(4.0f) shouldBe JsNumber(4.0)
    anyToJson(Some(4)) shouldBe JsNumber(4)
    anyToJson(Some(4.toByte)) shouldBe JsNumber(4)
    anyToJson(Some(4.toShort)) shouldBe JsNumber(4)
    anyToJson(Some(4L)) shouldBe JsNumber(4L)
    anyToJson(true) shouldBe JsBoolean(true)
    anyToJson("bla") shouldBe JsString("bla")
    anyToJson(List("bla", 4)) shouldBe JsArray(
      JsString("bla") :: JsNumber(4) :: Nil)
    anyToJson(Array("bla", 4)) shouldBe JsArray(
      JsString("bla") :: JsNumber(4) :: Nil)
    anyToJson(Map("key" -> "value")) shouldBe JsObject(
      Seq("key" -> JsString("value")))
    anyToJson(Map("key" -> Map("key2" -> "value"))) shouldBe JsObject(
      Seq("key" -> JsObject(Seq("key2" -> JsString("value")))))
  }

  @Test
  def testMapToYamlFile(): Unit = {
    val outputFile = File.createTempFile("test.", ".yml")
    outputFile.deleteOnExit()
    mapToYamlFile(Map("key" -> "value", "key2" -> Map("bla" -> 4)), outputFile)

    Source.fromFile(outputFile).getLines().toList shouldBe List("key: value",
                                                                "key2:",
                                                                "" +
                                                                  "  bla: 4",
                                                                "")
  }

  @Test
  def testListMapToYaml(): Unit = {
    val listMap = ListMap("a" -> 1, "d" -> 2, "c" -> 3, "e" -> 4, "b" -> 5)
    listMapToYaml(listMap) shouldBe
      """a: 1
        |d: 2
        |c: 3
        |e: 4
        |b: 5
        |""".stripMargin
    listMapToYaml(listMap) should not equal mapToYaml(listMap)
  }

  @Test
  def testMapToYamlToFileToYamlToMap(): Unit = {
    // This test makes sure the yaml conversion always works.
    val outputFile = File.createTempFile("test.", ".yaml")
    outputFile.deleteOnExit()
    val map = Map("key" -> "value", "key2" -> Map("bla" -> 4))
    mapToYamlFile(map, outputFile)
    yamlFileToMap(outputFile) shouldEqual (map)
  }

  @Test
  def testScalaListToJavaObjectArrayList(): Unit = {
    scalaListToJavaObjectArrayList(List()) shouldBe new util.ArrayList[Object]()

    case class Bla()
    val result = new util.ArrayList[Object]()
    result.add(Int.box(4))
    result.add(Char.box(4))
    result.add(Byte.box(4))
    result.add(Long.box(4L))
    result.add(Double.box(4.0))
    result.add(Float.box(4.0f))
    result.add(Boolean.box(true))
    result.add("bla")
    result.add(Bla())
    scalaListToJavaObjectArrayList(List(4,
                                        4.toChar,
                                        4.toByte,
                                        4L,
                                        4.0,
                                        4.0f,
                                        true,
                                        "bla",
                                        Bla())) shouldBe result
  }

  @Test
  def testNestedJavaHashMaptoScalaMap(): Unit = {
    val map = new java.util.LinkedHashMap[String, Any]()
    val map2 = new java.util.LinkedHashMap[String, Any]()
    map.put("key", "value")
    map2.put("key", "value")
    map.put("key2", map2)

    nestedJavaHashMaptoScalaMap(map) shouldBe Map(
      "key" -> "value",
      "key2" -> Map("key" -> "value"))
  }

  @Test
  def testFileToJson(): Unit = {
    fileToJson(resourceFile("/test.json")) shouldBe JsObject(
      Map("key" -> JsString("value")))
  }
}
