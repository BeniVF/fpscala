package gettingstarted

import org.scalatest.{Matchers, FlatSpec}
import Matchers._

class PolymorphicFunctionsSpec extends FlatSpec {

  import PolymorphicFunctions._

  val f = (x: Int, y: Int) => x + y
  val g = (x: Int, y: Int) => x * y


  it should "curry functions" in {
    curry(f)(1)(2) shouldBe f(1, 2)
    curry(g)(5)(6) shouldBe g(5, 6)
  }

  val h = (x: Int) => (y: Int) => x + y
  val i = (x: Int) => (y: Int) => x * y

  it should "uncurry a function" in {
    uncurry(h)(5,6) shouldBe h(5)(6)
    uncurry(i)(10,7) shouldBe i(10)(7)
  }

  val j = curry(f)(10)
  val k = curry(g)(3)

  it should "compose function" in {
    compose(j, k)(6) shouldBe j(k(6))
    compose(k, j)(9) shouldBe k(j(9))
  }




}
