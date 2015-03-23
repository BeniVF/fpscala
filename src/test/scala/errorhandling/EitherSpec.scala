package errorhandling

import org.scalatest.{Matchers, FlatSpec}
import Matchers._


class EitherSpec extends FlatSpec {

  it should "map" in {
    Left("there is a problem").map(_.toString + " add something") shouldBe Left("there is a problem")
    Right(1).map(_.toString) shouldBe Right("1")
  }

}
