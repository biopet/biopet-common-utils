package nl.biopet.utils

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

class UtilsTest extends BiopetTest {
  @Test
  def testSortAnyAny(): Unit = {
    //stub
    val one: Any = 1
    val two: Any = 2
    val text: Any = "hello"
    val text2: Any = "goodbye"

    sortAnyAny(one, two) shouldBe true
    sortAnyAny(two, one) shouldBe false
    sortAnyAny(text, text2) shouldBe false
    sortAnyAny(text2, text) shouldBe true
    sortAnyAny(one, text) shouldBe true
    sortAnyAny(text, one) shouldBe false
  }
}
