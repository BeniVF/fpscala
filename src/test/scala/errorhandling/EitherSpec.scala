package errorhandling

import org.scalatest.{Matchers, FlatSpec}
import Matchers._

class EitherSpec extends FlatSpec {
  import Either._

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))
  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))
  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

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
    mkPerson("", 10) shouldBe Left("Name is empty.")
    mkPerson("John", -10) shouldBe Left("Age is out of range.")
    import org.scalatest.Inside._
    inside(mkPerson("John", 10)) {
      case Right(Person(name, age)) =>
        name.value shouldBe "John"
        age.value shouldBe 10
    }
  }

  it should "traverse" in {
    traverse(List())(identity) shouldBe Right(List())
    traverse(List(1,2,3))(x => Right(x.toString)) shouldBe Right(List("1", "2", "3"))
    traverse(List(9,8,8))(x => Left("Always an error")) shouldBe Left("Always an error")
    traverse(List(3,5,6))(x => if (x%2==0) Right(x.toString) else Left(s"Even number: $x")) shouldBe Left("Even number: 3")
  }

  it should "sequence" in {
    sequence(List()) shouldBe Right(List())
    sequence(List(Right(1), Right(2), Right(5))) shouldBe Right(List(1, 2, 5))
    sequence(List(Right(1), Left("An error"), Right(5))) shouldBe Left("An error")
  }

}