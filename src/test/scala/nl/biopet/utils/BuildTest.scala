package nl.biopet.utils

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test


class BuildTest extends BiopetTest {
  @Test
  def testCompare(): Unit = {
    new Build("") > new Build("SNAPSHOT") shouldBe true
    new Build("alpha") < new Build("beta") shouldBe true
    new Build("rc-1") > new Build("beta") shouldBe true
    new Build("beta")  < new Build("") shouldBe true
    new Build("") < new Build("") shouldBe false
    new Build("alpha") == new Build("alpha")
    new Build("") == new Build("")
  }
}
