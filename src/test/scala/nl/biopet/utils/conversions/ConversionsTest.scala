package nl.biopet.utils.conversions

import java.util

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

class ConversionsTest extends BiopetTest {
  @Test
  def testAnyToList(): Unit = {
    val input1: Any = List(1,2)
    anyToList(input1) shouldBe input1

    val input2: Any = List("1",2)
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
    anyToStringList(List("1","2")) shouldBe List("1", "2")
    anyToStringList(List(1,2)) shouldBe List("1", "2")
  }

  @Test
  def testAnyToDoubleList(): Unit = {
    anyToDoubleList(List("1","2")) shouldBe List(1.0, 2.0)
    anyToDoubleList(List(1,2)) shouldBe List(1.0, 2.0)
    anyToDoubleList(List(1.0, 2.0)) shouldBe List(1.0, 2.0)
  }
}
