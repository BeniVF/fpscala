package fpinscala.state

import org.scalatest.FlatSpec

import org.scalatest.Matchers._

class RNGSpec extends FlatSpec {
  import RNG._

  it should "generate non negative ints" in {
    (1 to 100).foreach { i =>
      val rng = Simple(i)
      val (result, state) = nonNegativeInt(rng)
      rng should not be state
      if (result < 0)
        fail(s"It should be positive number $result")
    }
  }

  it should "generate double" in {
    (1 to 100).foreach { i =>
      val rng = Simple(i)
      val (result, state) = double(rng)
      rng should not be state
      result - result shouldBe 0
    }
  }

  it should "generate a tuple of (int, double)" in {
    (1 to 100).foreach { i =>
      val rng = Simple(i)
      val ((generatedInt: Int, generatedDouble: Double), state) = intDouble(rng)
      rng should not be state
    }
  }

  it should "generate a tuple of (double, int)" in {
    (1 to 100).foreach { i =>
      val rng = Simple(i)
      val ((generatedDouble: Double, generatedInt: Int), state) = doubleInt(rng)
      rng should not be state
    }
  }

  it should "generate a tuple of (double, double, double)" in {
    (1 to 100).foreach { i =>
      val rng = Simple(i)
      val ((first: Double, second: Double, third: Double), state) = double3(rng)
      rng should not be state
    }
  }

  it should "generate a list of ints" in {
    (1 to 100).foreach { i =>
      val rng = Simple(i)
      val (list, state) = ints(i)(rng)
      rng should not be state
      list should have size i
    }
  }

  it should "flatmap" in {
    (1 to 2).foreach { i =>
      val rng = Simple(i)
      val (result, state) = flatMap(nonNegativeInt)(n => ints(1))(rng)
      result.size should be>=(0)

      rng should not be state
    }
  }

  it should "map2" in {
    (1 to 100).foreach { i =>
      val rng = Simple(i)
      val (int: Int, state) = map2[Int, Int, Int](RNG.int, RNG.int)(_ + _)(rng)
      rng should not be state

      val ((first: Int, second: Int), state2) = map2[Int, Int, (Int, Int)](RNG.int, RNG.int) { (_, _) }(state)

      state should not be state2
      rng should not be state2
    }
  }

 

}
