package datastructures

import org.scalatest.{Matchers, FlatSpec}
import Matchers._


class ListSpec extends FlatSpec {

  import List._

  it should "be match" in {

    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    x shouldBe 3
  }

  it should "get the tail" in {
    tail(Nil) shouldBe Nil
    tail(List(1, 2)) shouldBe List(2)
    tail(List(1, 2, 3, 4)) shouldBe List(2, 3, 4)
  }

  it should "set the head" in {
    setHead(Nil, 1) shouldBe List(1)
    setHead(List(2, 3), 1) shouldBe List(1, 2, 3)
    setHead(List(2, 3, 4, 5), 1) shouldBe List(1, 2, 3, 4, 5)
  }

  it should "drop n elements" in {
    drop(Nil, 1) shouldBe Nil
    drop(List(2, 3), 1) shouldBe List(3)
    drop(List(2, 3, 4, 5), 2) shouldBe List(4, 5)
    drop(List(4, 3, 6, 5, 8, 1), 5) shouldBe List(1)
  }

  it should "drop while" in {
    dropWhile(Nil, (x: Int) => true) shouldBe Nil
    dropWhile(List(-1, -2, -3, 1, 2, 3), (x: Int) => true) shouldBe Nil
    dropWhile(List(-1, -2, -3, 1, 2, 3), (x: Int) => x < 0) shouldBe List(1, 2, 3)
  }

  it should "get init" in {
    init(Nil) shouldBe Nil
    init(List(1, 2)) shouldBe List(1)
    init(List(-1, -2, -3, 1, 2, 3)) shouldBe List(-1, -2, -3, 1, 2)
  }

  it should "calculate the length" in {
    length(Nil) shouldBe 0
    length(List(1, 2)) shouldBe 2
    length(List(-1, -2, -3, 1, 2, 3)) shouldBe 6
  }

  it should "fold left a list" in {
    val f = (acc: Int, current:Int) => current + acc
    foldLeft(Nil, 0)(f) shouldBe 0
    foldLeft(List(4), 0)(f) shouldBe 4
    foldLeft(List(4, 6, 5), 0)(f) shouldBe 15
  }

  it should "fold map a list" in {
    val f = (current:Int) => (current + current).toString
    map(Nil)(f) shouldBe Nil
    map(List(4))(f) shouldBe List("8")
    map(List(4, 6, 5))(f) shouldBe List("8", "12", "10")
  }

}
