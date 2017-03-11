package fpinscala.errorhandling

import org.scalatest._
import Matchers._

class EitherSpec extends FlatSpec {
  "Either" should "be able to map" in {
    Right(4).map(_ + 1) shouldBe Right(5)
    val left: Either[String, Int] = Left("dd")
    left.map { a: Int => a * 2 } shouldBe left
  }

  it should "be able to flatMap" in {
    (for {
      x1 <- Right("h")
      x2 <- Right("i")
    } yield x1 + x2) shouldBe Right("hi")

    (for {
      x1 <- Right(1)
      x2 <- Left("ddd")
    } yield x1 + x2) shouldBe Left("ddd")
  }

  it should "be able to orElse" in {
    Left("44") orElse Right(10) shouldBe Right(10)
    Right(10) orElse Left("44") shouldBe Right(10)
    Left(10) orElse Left("44") shouldBe Left("44")
  }

  it should "be able to map2" in {
    Right(3).map2(Right(4)) { _ + _ } shouldBe Right(7)
    Right(3).map2(Left("4")) { _ + _ } shouldBe Left("4")
    val left: Either[String, Int] = Left("dd")
    left.map2(Right(4)) { _ + _ } shouldBe Left("dd")
  }

  it should "be able to sequence" in {
    import Either._
    sequence(List(Right(3), Right(4))) shouldBe Right(List(3, 4))
    sequence(List(Left("dd"), Right(4))) shouldBe Left("dd")
    sequence(List(Right(4), Left("dda"))) shouldBe Left("dda")
  }

  it should "be able to traverse" in {
    import Either._
    traverse(List(3, 4)) { a => Right(a + 1) } shouldBe Right(List(4, 5))
    traverse(List(3, 4)) { a => if (a % 3 == 0) Left("boom") else Right(a * 2) } shouldBe Left("boom")
  }

}
