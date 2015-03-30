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
      value should be >=0
      newRng should not be(rng)
    }
  }

  it should "generate a random double between 0 and 1" in {
    generateRNGs { rng =>
      val (value, newRng) = double(rng)
      value should be >= 0.0
      value should be <= 1.0
      newRng should not be(rng)
    }
  }

  it should "generate a random pair of Int and Double" in {
    generateRNGs { rng =>
      val ((intValue, doubleValue), newRng) = intDouble(rng)
      intValue should be >= 0
      doubleValue should be >= 0.0
      doubleValue should be <= 1.0
      newRng should not be(rng)
    }
  }

  it should "generate a random pair of Double and Int" in {
    generateRNGs { rng =>
      val ((doubleValue, intValue), newRng) = doubleInt(rng)
      intValue should be >= 0
      doubleValue should be >= 0.0
      doubleValue should be <= 1.0
      newRng should not be(rng)
    }
  }

}
