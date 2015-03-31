package state

import org.scalatest.Matchers._
import org.scalatest.{FlatSpec}

class RandSpec extends FlatSpec {
  import RNG._

  def generateRand(test : (RNG, Int) => Unit): Unit = {
    for (i <- 1 to 100000) {
      test(Simple(i), i)
    }
  }

  it should "generate random integer" in {
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

  it should "generate a random with map2 combinator" in {
    generateRand {
      (rng, i) =>
        val ((intValue, doubleValue), newRng) = map2(positiveMax(i),doubleRand){(_,_)}(rng)
        assertValidRange(intValue, i)
        assertValidDouble(doubleValue)
        assertNewStateIsGenerated(rng, newRng)
    }
  }

  it should "generate a random with sequence combinator" in {
    generateRand {
      (rng, i) =>
        val (list, newRng) = sequence(List(positiveMax(i), positiveMax(i*10), positiveMax(i*100)))(rng)
        val first  = list.head
        val second  = list.tail.head
        val third  = list.last

        assertValidRange(first, i)
        assertValidRange(second, i*10)
        assertValidRange(third, i*100)
        assertNewStateIsGenerated(rng, newRng)
    }
  }

  it should "generate a list of random integers" in {
    generateRand { (rng, i) =>
      val (integers, newRng) = intsRand(10)(rng)
      integers.size shouldBe 10
      integers.foreach(assertValidInt)
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

  def assertValidInt(intValue: Int): Unit = {
    intValue should be >= Integer.MIN_VALUE
    intValue should be <= Integer.MAX_VALUE
  }

}
