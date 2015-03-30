package state

import org.scalatest.Matchers._
import org.scalatest.{FlatSpec, Matchers}

class RandSpec extends FlatSpec {
  import RNG._

  def generateRand(test : (RNG, Int) => Unit): Unit = {
    for (i <- 0 to 100000) {
      test(Simple(i), i)
    }
  }

  it should "generate a list of random integers" in {
    generateRand {
      (rng, i) =>
      val (value, newRng) = positiveMax(i)(rng)
        assertValidRange(value, i)
        assertNewStateIsGenerated(rng, newRng)
    }
  }


  it should "generate a random double between 0 and 1" in {
    generateRand {
      (rng, _) =>
      val (value, newRng) = doubleRand(rng)
      assertValidDouble(value)
      assertNewStateIsGenerated(rng, newRng)
    }
  }

  def assertValidRange(value: Int, max: Int): Unit = {
    value should be >= 0
    value should be <= max
  }

  def assertNewStateIsGenerated(rng: RNG, newRng: RNG): Unit =
    newRng should not be (rng)

  def assertValidDouble(value: Double): Unit = {
    value should be >= 0.0
    value should be <= 1.0
  }

}
