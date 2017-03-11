package fpinscala.state

import org.scalatest.Matchers._
import org.scalatest.FlatSpec

class CandyMachineSpec extends FlatSpec {

  import State._

  "Candy machine" should "unlock if there’s any candy left when inserting a coin into a locked machine" in {
    val initMachine = Machine(locked = true, 1, 0)
    val (_, endMachine) = simulateMachine(List(Coin)).run(initMachine)
    endMachine shouldBe Machine(locked = false, candies = 1, coins = 1)
  }

  it should "does nothing when turning the knob on a locked machine inserting a coin into an unlocked machine" in {
    val initMachine = Machine(locked = false, 1, 0)
    val (_, endMachine) = simulateMachine(List(Coin, Coin, Coin)).run(initMachine)
    endMachine shouldBe Machine(locked = false, candies = 1, coins = 3)
  }

  it should "not unlock if there is not any candy left when inserting a coin into a locked machine" in {
    val initMachine = Machine(locked = true, 0, 0)
    val (_, endMachine) = simulateMachine(List(Coin)).run(initMachine)
    endMachine shouldBe Machine(locked = true, candies = 0, coins = 0)
  }

  it should " dispense candy and become locked when turning the knob on an unlocked machine" in {
    val initMachine = Machine(locked = false, candies = 1, coins = 1)
    val (_, endMachine) = simulateMachine(List(Turn)).run(initMachine)
    endMachine shouldBe Machine(locked = true, candies = 0, coins = 1)
  }

  it should "does nothing when turning the knob on a locked machine" in {
    val initMachine = Machine(locked = true, candies = 0, coins = 0)
    val (_, endMachine) = simulateMachine(List(Turn)).run(initMachine)
    endMachine shouldBe Machine(locked = true, candies = 0, coins = 0)
  }

  it should "dispense 2 candies" in {
    val initMachine = Machine(locked = true, 4, 0)
    val (_, endMachine) = simulateMachine(List(Coin, Turn, Coin, Turn)).run(initMachine)
    endMachine shouldBe Machine(locked = true, 2, 2)
  }

  it should "dispense 4 candies" in {
    val initMachine = Machine(true, 4, 0)
    val (_, endMachine) = simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(initMachine)
    endMachine shouldBe Machine(true, 0, 4)
  }

  it should "do nothing when inserting a coin into a locked machine if there is not any candy left" in {
    val initMachine = Machine(true, 0, 0)
    val (_, endMachine) = simulateMachine(List(Coin)).run(initMachine)
    endMachine shouldBe initMachine
  }

  it should "ignore all inputs when it is out of candy" in {
    val initMachine = Machine(true, 0, 0)
    val (_, endMachine) = simulateMachine(List(Coin, Turn, Coin, Turn)).run(initMachine)
    endMachine shouldBe initMachine
  }
}

