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
import org.testng.annotations.Test

class UtilsTest extends BiopetTest {
  @Test
  def testSortAnyAny(): Unit = {
    //stub
    val one: Any = 1
    val two: Any = 2
    val three: Any = 3.0
    val text: Any = "hello"
    val text2: Any = "goodbye"

    sortAnyAny(one, two) shouldBe true
    sortAnyAny(two, one) shouldBe false
    sortAnyAny(text, text2) shouldBe false
    sortAnyAny(text2, text) shouldBe true
    sortAnyAny(one, text) shouldBe true
    sortAnyAny(text, one) shouldBe false

    sortAnyAny(one, three) shouldBe true
    sortAnyAny(three, one) shouldBe false
  }

  @Test
  def testTextToSize(): Unit = {
    textToSize("10") shouldBe 10L

    textToSize("1k") shouldBe 1024L
    textToSize("1m") shouldBe (1024L * 1024L)
    textToSize("1g") shouldBe (1024L * 1024L * 1024L)

    textToSize("1K") shouldBe 1024L
    textToSize("1M") shouldBe (1024L * 1024L)
    textToSize("1G") shouldBe (1024L * 1024L * 1024L)
  }

  @Test
  def testCamelize(): Unit = {
    camelize("bla_bla") shouldBe "BlaBla"
  }

  @Test
  def testCamelizeToWords(): Unit = {
    camelizeToWords("BlaBla") shouldBe List("Bla", "Bla")
    camelizeToWords("Bla") shouldBe List("Bla")
    camelizeToWords("") shouldBe List()
  }

  @Test
  def testUnCamelize(): Unit = {
    unCamelize("BlaBla") shouldBe "bla_bla"
  }

  @Test
  def testBiopetProperties(): Unit = {
    loadBiopetProperties()
    System.getProperty("bla") shouldBe "bla"
  }
}
