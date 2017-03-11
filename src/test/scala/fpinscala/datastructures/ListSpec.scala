package fpinscala.datastructures
import org.scalatest._
import Matchers._

class ListSpec extends FlatSpec {
  import List._

  "List" should "get the tail" in {
    tail(Nil) shouldBe Nil
    tail(List(3)) shouldBe Nil
    tail(List(1, 2, 3)) shouldBe List(2, 3)
    tail(List(5, 6, 7, 1, 2, 3)) shouldBe List(6, 7, 1, 2, 3)
  }

  it should "set the head" in {
    setHead(Nil, "a") shouldBe List("a")
    setHead(List("b", "c"), "a") shouldBe List("a", "c")
    setHead(List(1, 2, 3), 6) shouldBe List(6, 2, 3)
  }

  it should "drop n elements" in {
    drop(List(1, 2, 3), 0) shouldBe List(1, 2, 3)
    drop(Nil, 1) shouldBe Nil
    drop(List(1, 2, 3), 1) shouldBe List(2, 3)
    drop(List("1", "2a", "3b", "5c"), 2) shouldBe List("3b", "5c")
    drop(List("1", "2a", "3b", "5c"), 4) shouldBe Nil
    drop(List("1", "2a", "3b", "5c"), 5) shouldBe Nil
  }

  it should "drop while condition" in {
    def isZero(a: Int): Boolean = a == 0
    def isEven(a: Int): Boolean = (a % 2) == 0
    def alwaysTrue(a: Int): Boolean = true

    dropWhile[Int](List(1, 2, 3), isZero) shouldBe List(1, 2, 3)
    dropWhile[Int](List(2, 4, 1, 3), isEven) shouldBe List(1, 3)
    dropWhile[Int](List(1, 2, 3), alwaysTrue) shouldBe Nil
  }

  it should "get the init" in {
    init(Nil) shouldBe Nil
    init(List(1)) shouldBe Nil
    init(List(1, 2)) shouldBe List(1)
    init(List("1", "2a", "3b", "5c")) shouldBe List("1", "2a", "3b")
  }

  it should "get the length" in {
    length(Nil) shouldBe 0
    length(List(1)) shouldBe 1
    length(List(1, 2, 3, 4, 5)) shouldBe 5
  }

  it should "fold left" in {
    foldLeft(Nil, 0)((a: Int, b:Int) => a + b) shouldBe 0
    foldLeft(List(1, 2, 3, 4), 1)((b: Int, a:Int) => a * b) shouldBe 24
    foldLeft(List(1, 2, 3, 4), "")((b: String, a:Int) => a.toString + b) shouldBe "4321"
  }

  it should "map" in {
    map(Nil)(_.toString) shouldBe Nil
    map(List(1, 2, 3, 4)){a: Int => 2*a} shouldBe List(2, 4, 6, 8)
  }

}
