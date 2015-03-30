package state

import org.scalatest.{Matchers, FlatSpec}
import Matchers._

class StateSpec extends FlatSpec {
  import RNG._

  def generateRNGs(test : RNG => Unit): Unit = {
    for (i <- 0 to 10000) {
      test(Simple(i))
    }
  }

  it should "generate a random positive integer" in {
    generateRNGs { rng =>
      val (value, newRng) = nonNegativeInt(rng)
      assertValidInt(value)
      assertNewStateIsGenerated(rng, newRng)
    }
  }

  def assertNewStateIsGenerated(rng: RNG, newRng: RNG): Unit = {
    newRng should not be (rng)
  }

  it should "generate a random double between 0 and 1" in {
    generateRNGs { rng =>
      val (value, newRng) = double(rng)
      assertValidDouble(value)
      assertNewStateIsGenerated(rng, newRng)
    }
  }

  it should "generate a random pair of Int and Double" in {
    generateRNGs { rng =>
      val ((intValue, doubleValue), newRng) = intDouble(rng)
      assertValidInt(intValue)
      assertValidDouble(doubleValue)
      assertNewStateIsGenerated(rng, newRng)
    }
  }

  it should "generate a random pair of Double and Int" in {
    generateRNGs { rng =>
      val ((doubleValue, intValue), newRng) = doubleInt(rng)
      assertValidInt(intValue)
      assertValidDouble(doubleValue)
      assertNewStateIsGenerated(rng, newRng)
    }
  }



  it should "generate a random pair of three Doubles" in {
    generateRNGs { rng =>
      val ((first, second, third), newRng) = double3(rng)
      assertValidDouble(first)
      assertValidDouble(second)
      assertValidDouble(third)
      assertNewStateIsGenerated(rng, newRng)
    }
  }

  def assertValidInt(intValue: Int): Unit = {
    intValue should be >= 0
  }

  def assertValidDouble(value: Double): Unit = {
    value should be >= 0.0
    value should be <= 1.0
  }
}
