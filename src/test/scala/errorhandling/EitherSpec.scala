package errorhandling

import org.scalatest.{Matchers, FlatSpec}
import Matchers._


class EitherSpec extends FlatSpec {

  it should "map" in {
    Left("there is a problem").map(_.toString + " add something") shouldBe Left("there is a problem")
    Right(1).map(_.toString) shouldBe Right("1")
  }

  it should "flatMap" in {
    val result = for {
      x <- Right(5)
      y <- Right(10)
    } yield x + y
    result shouldBe Right(15)
    def problem(problem: String): Either[String, Int] = {
      Left(problem)
    }
    val error = for {
      x <- Right(5)
      y <- problem("A problem!!!")
    } yield x + y
    error shouldBe error
  }

  it should "orElse" in {
    Left("this is a problem") orElse Right(21) shouldBe Right(21)
    Right(44) orElse Right(21) shouldBe Right(44)
  }

  it should "map2" in {
    Right(44).map2(Left(21))(_+_) shouldBe Left(21)
    Right(44).map2(Right(21))(_+_) shouldBe Right(65)
  }

}
