package datastructures

import org.scalatest.Matchers._
import org.scalatest.{FlatSpec, Matchers}


class TreeSpec extends FlatSpec {

  import datastructures.Tree._


  it should "get the size" in {
    size(Leaf(1)) shouldBe 1
    size(Branch(Leaf(1), Leaf(2))) shouldBe 3

    size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2))) ) shouldBe 7
  }

  it should "get the max" in {
    maximum(Leaf(1)) shouldBe Some(1)
    maximum(Branch(Leaf(1), Leaf(2))) shouldBe Some(2)

    maximum(Branch(Branch(Leaf(1), Leaf(5)), Branch(Leaf(1), Leaf(2))) ) shouldBe Some(5)

    maximum(Branch(Branch(Leaf(-1), Leaf(-5)), Branch(Leaf(-1), Leaf(-8))) ) shouldBe Some(-1)
  }


}