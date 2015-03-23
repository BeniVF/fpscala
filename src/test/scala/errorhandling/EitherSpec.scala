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

}
