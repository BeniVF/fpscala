package fpinscala.parallelism

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class FirstParSpec extends FlatSpec {

  import FirstPar._
  it should "create a unit Par" in {
    FirstPar.run(unit(10)) shouldBe 10
  }

  it should "create a map2 Par" in {
    FirstPar.run(map2(unit(10), unit("5")) { (a, b) => a * b.toInt }) shouldBe 50
  }

  it should "run in different thread using fork" in {
    FirstPar.run(map2(
      fork(unit {
        Thread.sleep(100)
        val name = Thread.currentThread.getName
        println(s"Executed $name")
        name
      }),
      fork(unit{
        Thread.sleep(10)
        val name = Thread.currentThread.getName
        println(s"Executed $name")
        name
      })
    ) { (a, b) => a == b }) shouldBe false
  }

}

