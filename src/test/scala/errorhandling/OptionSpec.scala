package errorhandling

import org.scalatest.{Matchers, FlatSpec}
import Matchers._

class OptionSpec extends FlatSpec {
  it should "get or else the value" in {
    None.getOrElse("No value!") shouldBe "No value!"
    Some(1).getOrElse(3) shouldBe 1
    Some("value").getOrElse("No value!") shouldBe "value"
  }

}
