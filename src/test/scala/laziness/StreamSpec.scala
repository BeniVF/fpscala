package laziness

import org.scalatest.{Matchers, FlatSpec}
import Matchers._

class StreamSpec extends FlatSpec {

  import Stream._

  it should "take n elements" in {
    Stream().take(10).toList shouldBe List()
    Stream(1, 2, 3, 4).take(2).toList shouldBe List(1, 2)
    ones.take(5).toList shouldBe List(1, 1, 1, 1, 1)
  }

  it should "drop n elements" in {
    Stream().drop(10).toList shouldBe List()
    Stream(1, 2, 3, 4).drop(1).toList shouldBe List(2, 3, 4)
    Stream(1, 2, 3, 4, 5).drop(4).toList shouldBe List(5)
    ones.drop(10).take(1).toList shouldBe List(1)
  }

  it should "take while a condition" in {
    Stream(1, 2, 3, 4).takeWhile(_ % 2 != 0).toList shouldBe List(1)
    Stream(2, 4, 6, 1).takeWhile(_ % 2 == 0).toList shouldBe List(2, 4, 6)
  }

  it should "forAll" in {
    Stream().forAll(_ => false) shouldBe true
    Stream(1, 2, 3).forAll(_ => false) shouldBe false
    Stream(2, 4, 6, 8).forAll(_ % 2 == 0) shouldBe true
  }

  it should "headOption" in {
    Stream().headOption shouldBe None
    Stream(4, 5).headOption shouldBe Some(4)
    ones.headOption shouldBe Some(1)
  }

  it should "map" in {
    Stream().map(identity).toList shouldBe List()
    Stream(4, 5).map(_ * 2).toList shouldBe List(8, 10)
  }

  it should "filter" in {
    Stream().filter(_ => true).toList shouldBe List()
    Stream(4, 9).filter(_ % 3 == 0).toList shouldBe List(9)
    Stream(4, 9, 8).filter(_ % 2 == 0).toList shouldBe List(4, 8)
  }

  it should "append" in {
    (Stream() append Stream()).toList shouldBe List()
    (Stream(1, 2) append Stream()).toList shouldBe List(1, 2)
    (Stream(1, 2) append Stream(3)).toList shouldBe List(1, 2, 3)
    (Stream(5, 6) append Stream(9, 10, 23)).toList shouldBe List(5, 6, 9, 10, 23)
  }

  it should "flatMap" in {
    Stream().flatMap{a: Int => Stream(a+1)}.toList shouldBe List()
    val result = for {
      x <- Stream(1, 2)
      y <- Stream(4, 5, 6)
    } yield x + y
    result.toList shouldBe List(5, 6, 7, 6, 7, 8)
  }


  it should "create an infinite constant" in {
    constant(10).take(2).toList shouldBe List(10, 10)
  }
}
