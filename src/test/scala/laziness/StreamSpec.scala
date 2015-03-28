package laziness

import org.scalatest.{Matchers, FlatSpec}
import Matchers._

class StreamSpec extends FlatSpec {

  import Stream._
  import errorhandling._
  import errorhandling.Option._

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

  it should "create an infinite stream of integers" in {
    from(0).take(5).toList shouldBe List(0, 1, 2, 3, 4)
    from(10).take(2).toList shouldBe List(10, 11)
  }

  it should "calculate fibonacci" in {
    fibs.take(10).toList shouldBe List(0,1,1,2,3,5,8,13,21,34)
  }

  it should "unfold" in {
    unfold(10)( x => if (x<100) Some((x+x, x+x)) else None).toList shouldBe List(20, 40, 80, 160)
    unfold(2)( x => Some((x*x, x*x))).take(4).toList shouldBe List(4, 16, 256, 65536)
  }

  it should "zipWith" in {
    Stream().zipWith(Stream()){(x:Int, y:Int) => x+y}.toList shouldBe List()
    Stream(1, 2).zipWith(Stream(2, 4)){_+_}.toList shouldBe List(3,6)
    Stream(2, 3).zipWith(Stream(5, 6)){_*_}.toList shouldBe List(10,18)
    Stream(2, 3).zipWith(Stream(5, 6, 6)){_*_}.toList shouldBe List(10,18)
  }

  it should "zip" in {
    Stream().zip(Stream()).toList shouldBe List()
    Stream(1, 2).zip(Stream(2, 4)).toList shouldBe List((1,2), (2,4))
  }

  it should "zipWithAll" in {
    Stream().zipWithAll(Stream()){(_,_)}.toList shouldBe List()
    Stream(1, 2).zipWithAll(Stream(2, 4)){map2(_, _)(_+_).getOrElse(0)}.toList shouldBe List(3,6)
    Stream(2, 3).zipWithAll(Stream(5, 6, 3)){(x, y) => (map2(x, y)(_*_) orElse x orElse y).getOrElse(0)}.toList shouldBe List(10,18, 3)
    Stream(2, 3).zipWithAll(Stream(5, 6, 7)){map2(_, _)(_*_).getOrElse(0)}.toList shouldBe List(10,18, 0)
  }

  it should "zipAll" in {
    Stream().zipAll(Stream()).toList shouldBe List()
    Stream(1, 2).zipAll(Stream(2, 4, 5)).toList shouldBe List((Some(1),Some(2)), (Some(2),Some(4)), (None, Some(5)))
    Stream("a", "b").zipAll(Stream("c")).toList shouldBe List((Some("a"),Some("c")), (Some("b"), None))
  }

}
