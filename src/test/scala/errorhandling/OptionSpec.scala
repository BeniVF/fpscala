package errorhandling

import org.scalatest.{Matchers, FlatSpec}
import Matchers._

class OptionSpec extends FlatSpec {
  it should "get or else the value" in {
    None.getOrElse("No value!") shouldBe "No value!"
    Some(1).getOrElse(3) shouldBe 1
    Some("value").getOrElse("No value!") shouldBe "value"
  }

  it should "map" in {
    None.map(identity) shouldBe None
    Some(3).map(_*10) shouldBe Some(30)
    Some(3).map(_+10) shouldBe Some(13)
  }

  it should "flat map" in {
    for {
      x <-Some(1)
      y <-Some(2)
    } yield x+y shouldBe 3

    for {
      x <-Some(1)
      y <-None
    } yield x+y shouldBe None

  }

}
