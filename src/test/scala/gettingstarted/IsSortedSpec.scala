package gettingstarted

import org.scalatest.{Matchers, FlatSpec}
import Matchers._

class IsSortedSpec extends FlatSpec {
  import PolymorphicFunctions._

  private val gt = (x: Int, y: Int) => x > y

  "an empty array" should "be sorted" in {
    isSorted(Array(), gt ) shouldBe true
  }

  "an array with one element" should "be sorted" in {
    isSorted(Array(1), gt ) shouldBe true
  }

  "an array with two disordered elements" should "not be sorted" in {
    isSorted(Array(2, 1), gt ) shouldBe false
  }

  "an array with three disordered elements" should "not be sorted" in {
    isSorted(Array(3, 2, 1), gt ) shouldBe false
  }

  "an array with four disordered elements" should "not be sorted" in {
    isSorted(Array(3, 5, 2, 1), gt ) shouldBe false
  }

  "an array with five ordered elements" should "be sorted" in {
    isSorted(Array(1, 5, 7, 8, 9), gt ) shouldBe true
  }
  

}
