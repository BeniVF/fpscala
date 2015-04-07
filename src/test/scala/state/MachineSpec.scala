package state

import org.scalatest.Matchers._
import org.scalatest.FlatSpec

class MachineSpec extends FlatSpec {
  import State._

  "Machine" should "dispense 2 candies" in {
    val initMachine = Machine(true, 4, 0)
    val (_, endMachine) = simulateMachine(List(Coin, Turn, Coin, Turn)).run(initMachine)
    endMachine shouldBe Machine(true, 2, 2)
  }

  it should "dispense 4 candies" in {
    val initMachine = Machine(true, 4, 0)
    val (_, endMachine) = simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(initMachine)
    endMachine shouldBe Machine(true, 0, 4)
  }

  it should "unlock the machine when inserting a coin into a locked machine if there is any candy left" in {
    val initMachine = Machine(true, 1, 0)
    val (_, endMachine) = simulateMachine(List(Coin)).run(initMachine)
    endMachine shouldBe Machine(false, 1, 1)
  }

  it should "dispense candy and become locked when turning the knob on an unlocked machine" in {
    val initMachine = Machine(false, 1, 1)
    val (_, endMachine) = simulateMachine(List(Turn)).run(initMachine)
    endMachine shouldBe Machine(true, 0, 1)
  }

  it should "do nothing when turning the knob on an unlocked machine" in {
    val initMachine = Machine(false, 0, 1)
    val (_, endMachine) = simulateMachine(List(Turn)).run(initMachine)
    endMachine shouldBe initMachine
  }

  it should "do nothing when turning the knob on a locked machine" in {
    val initMachine = Machine(true, 0, 1)
    val (_, endMachine) = simulateMachine(List(Turn)).run(initMachine)
    endMachine shouldBe initMachine
  }

  it should "do nothing when inserting a coin into a locked machine if there is not any candy left" in {
    val initMachine = Machine(true, 0, 0)
    val (_, endMachine) = simulateMachine(List(Coin)).run(initMachine)
    endMachine shouldBe initMachine
  }

  it should "do nothing when inserting a coin into an unlocked machine" in {
    val initMachine = Machine(false, 0, 0)
    val (_, endMachine) = simulateMachine(List(Coin)).run(initMachine)
    endMachine shouldBe initMachine
  }

  it should "ignore all inputs when it is out of candy" in {
    val initMachine = Machine(true, 0, 0)
    val (_, endMachine) = simulateMachine(List(Coin, Turn, Coin, Turn)).run(initMachine)
    endMachine shouldBe initMachine
  }


}
