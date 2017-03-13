package fpinscala.parallelism

import java.util.concurrent.{Executors, TimeUnit}

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class ParSpec extends FlatSpec {

  import Par._
  import Executors._

  val singleThread = newFixedThreadPool(1)
  val multipleThreads = newFixedThreadPool(100)

  it should "create a unit" in {
    Par.run(singleThread)(unit(10)).get shouldBe 10
  }

  it should "map2" in {
    Par.run(multipleThreads)(map2(unit(10), unit("5")) { (a, b) => a * b.toInt })
      .get shouldBe 50
  }

  it should "map" in {
    Par.run(multipleThreads)(map(unit(10)) { (a) => a * 10 })
      .get shouldBe 100
  }


  it should "sortMap" in {
    Par.run(multipleThreads)(sortPar(unit(List(4, 1, 2, 5, 7, 6, 3))))
      .get shouldBe (1 to 7)
  }


  it should "choice between two Par" in {
    Par.run(multipleThreads)(choice(unit(true))(unit(3), unit(5)))
      .get shouldBe 3

    Par.run(multipleThreads)(choice(unit(false))(unit(3), unit(5)))
      .get shouldBe 5
  }

  it should "run in different thread using fork" in {
    Par.run(multipleThreads)(map2(
      fork(unit {
        sleep(100)
      }),
      fork(unit {
        sleep(10)
      })
    ) { (a, b) => a == b }).get shouldBe false
  }

  it should "execute an async function" in {
    asyncF { x: Long =>
      sleep(x)
      x.toString
    }(10L).apply(multipleThreads).get shouldBe "10"
  }

  it should "sequence" in {
    val threads = 1 to 10
    val tasks = threads.map(asyncF { time: Int =>
      sleep(time)
      time.toString
    }(_))
    val expectedResult = threads.map(_.toString).toList

    sequence_simple(tasks.toList)(multipleThreads).get(2, TimeUnit.SECONDS) shouldBe expectedResult
    sequenceRight(tasks.toList)(multipleThreads).get(2, TimeUnit.SECONDS) shouldBe expectedResult
    sequenceBalanced(tasks)(multipleThreads).get(2, TimeUnit.SECONDS) shouldBe expectedResult
    sequence(tasks.toList)(multipleThreads).get(2, TimeUnit.SECONDS) shouldBe expectedResult
  }

  it should "parFilter" in {
    val isEven : Int => Boolean = _ % 2 == 0
    parFilter((1 to 10).toList)(isEven)(multipleThreads).get shouldBe (1 to 10).toList.filter(isEven)
  }

  it should "choice" in {
    choiceN(unit(1))((1 to 10).map(unit).toList)(multipleThreads).get shouldBe 2
  }

  private def sleep(milliseconds: Long): String = {
    Thread.sleep(milliseconds)
    val name = Thread.currentThread.getName
    println(s"Executing $name")
    name
  }
}

