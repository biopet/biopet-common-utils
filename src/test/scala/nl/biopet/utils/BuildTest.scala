package nl.biopet.utils

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test
import Build.fromString

class BuildTest extends BiopetTest {
  @Test
  def testCompare(): Unit = {
    fromString("") > fromString("SNAPSHOT") shouldBe true
  }
}
