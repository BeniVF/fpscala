package datastructures

import org.scalatest.Matchers._
import org.scalatest.{FlatSpec, Matchers}


class TreeSpec extends FlatSpec {

  import datastructures.Tree._

  it should "get the size" in {
    size(Leaf(1)) shouldBe 1
    size(Branch(Leaf(1), Leaf(2))) shouldBe 3
    size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2)))) shouldBe 7
  }

  it should "get the max" in {
    maximum(Leaf(1)) shouldBe Some(1)
    maximum(Branch(Leaf(1), Leaf(2))) shouldBe Some(2)
    maximum(Branch(Branch(Leaf(1), Leaf(5)), Branch(Leaf(1), Leaf(2)))) shouldBe Some(5)
    maximum(Branch(Branch(Leaf(-1), Leaf(-5)), Branch(Leaf(-1), Leaf(-8)))) shouldBe Some(-1)
  }

  it should "get the depth" in {
    depth(Leaf(1)) shouldBe 1
    depth(Branch(Leaf(1), Leaf(2))) shouldBe 2
    depth(Branch(Branch(Leaf(1), Leaf(5)), Leaf(2))) shouldBe 3
  }

  it should "map the tree" in {
    val f: (Int) => String = x => (x * x).toString
    map(Leaf(2))(f) shouldBe Leaf("4")
    map(Branch(Leaf(4), Leaf(2)))(f) shouldBe Branch(Leaf("16"), Leaf("4"))
    map(Branch(Branch(Leaf(5), Leaf(-5)), Branch(Leaf(-2), Leaf(-3))))(f) shouldBe
      Branch(Branch(Leaf("25"), Leaf("25")), Branch(Leaf("4"), Leaf("9")))
  }

  it should "fold the tree" in {
    val f = (a: Int) => a * 2
    val g = (b1: Int, b2:Int) => b1+b2
    fold(Leaf(4))(f)(g) shouldBe 8
    fold(Branch(Leaf(5), Leaf(2)))(f)(g) shouldBe 14

  }


}