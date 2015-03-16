package gettingstarted

import org.scalatest.{Matchers, FlatSpec}
import Matchers._

class FibonacciSpec extends FlatSpec {

  import MyModule._

  "fib(0)" should "0" in {
    fib(0) shouldBe 0
  }


  "fib(1)" should "1" in {
    fib(1) shouldBe 1
  }

  "fib(2)" should "1" in {
    fib(2) shouldBe 1
  }

  "fib(3)" should "2" in {
    fib(3) shouldBe 2
  }

  "fib(4)" should "3" in {
    fib(4) shouldBe 3
  }

  "fib(5)" should "5" in {
    fib(5) shouldBe 5
  }

  "fib(6)" should "8" in {
    fib(6) shouldBe 8
  }

  "fib(10)" should "55" in {
    fib(10) shouldBe 55
  }

  "fib(20)" should "6765" in {
    fib(20) shouldBe 6765
  }



}
