package state

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class StateSpec extends FlatSpec {

  it should "generate random integer" in {
    val state: State[Int, Int] = State(s => (s, s + 1))
    state.map(_ * 2).run(1) shouldBe(2, 2)
  }


}
