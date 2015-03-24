package laziness

import org.scalatest.{Matchers, FlatSpec}
import Matchers._

class StreamSpec extends FlatSpec{
  import Stream._

  it should "take n elements" in {
    Stream().take(10).toList shouldBe List()
    Stream(1, 2, 3, 4).take(2).toList shouldBe List(1,2)
    ones.take(5).toList shouldBe List(1,1,1,1,1)
  }

  it should "drop n elements" in {
    Stream().drop(10).toList shouldBe List()
    Stream(1,2,3,4).drop(1).toList shouldBe List(2, 3, 4)
    Stream(1,2,3,4,5).drop(4).toList shouldBe List(5)
    ones.drop(10).take(1).toList shouldBe List(1)
  }

}
