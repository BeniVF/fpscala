package state

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class StateSpec extends FlatSpec {

  it should "map" in {
    val state: State[Int, Int] = State(s => (s, s + 1))
    state.map(_ * 2).run(1) shouldBe(2, 2)
  }

  it should "flatMap" in {
    val result = for {
      x <- State[Int, Int](s => (s, s + 1))
      y <- State[Int, Int](s => (s, s + 2))
    } yield x + y
    result.run(1) shouldBe (3, 4)
  }

  it should "map2" in {
    val state: State[Int, Int] = State(s => (s, s + 1))
    state.map2(state)(_ * _).run(2) shouldBe (6, 4)
  }


  it should "modify the state" in {
    import State._
    modify{s: Int => s + 1 }.run(7) shouldBe ((), 8)
    modify{s: Int => s * 2 }.run(5) shouldBe ((), 10)
  }


}
