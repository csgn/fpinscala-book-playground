package state

import state.Candy.simulateMachine

class CandySuite extends munit.FunSuite:
  val coinInput = List(Input.Coin)
  val turnInput = List(Input.Turn)
  val coinAndTurnInput = coinInput ::: turnInput
  val coinAndTurnInput2 = coinAndTurnInput ::: coinAndTurnInput

  val noCandiesMachine = Machine(false, 0, 0)
  val lockedMachine = Machine(true, 1, 1)
  val unlockedMachine = Machine(false, 5, 3)

  test("Candy: a machine that's out of candy"):
    val ((coins, candies), machineNext) =
      simulateMachine(coinInput).run(noCandiesMachine)

    assertEquals(candies, 0)
    assertEquals(coins, machineNext.coins)
    assertEquals(machineNext, noCandiesMachine)

  test("Candy: inserting a coin into a locked machine"):
    val ((coins, candies), machineNext) =
      simulateMachine(coinInput).run(lockedMachine)

    assertEquals(candies, lockedMachine.candies)
    assertEquals(coins, lockedMachine.coins + 1)
    assertEquals(machineNext, Machine(false, candies, coins))

  test("Candy: turning the knob on a locked machine"):
    val ((coins, candies), machineNext) =
      simulateMachine(turnInput).run(lockedMachine)

    assertEquals(candies, lockedMachine.candies)
    assertEquals(coins, lockedMachine.coins)
    assertEquals(machineNext, lockedMachine)

  test("Candy: inserting a coin into an unlocked machine"):
    val ((coins, candies), machineNext) =
      simulateMachine(coinInput).run(unlockedMachine)

    assertEquals(candies, unlockedMachine.candies)
    assertEquals(coins, unlockedMachine.coins)
    assertEquals(machineNext, unlockedMachine)

  test("Candy: turning the knob on an unlocked machine"):
    val ((coins, candies), machineNext) =
      simulateMachine(turnInput).run(unlockedMachine)

    assertEquals(candies, unlockedMachine.candies - 1)
    assertEquals(coins, unlockedMachine.coins)
    assertEquals(machineNext, Machine(true, candies, coins))

  test("Candy: empty inputs"):
    val ((coins, candies), machineNext) =
      simulateMachine(List.empty).run(unlockedMachine)

    assertEquals(candies, unlockedMachine.candies)
    assertEquals(coins, unlockedMachine.coins)
    assertEquals(machineNext, unlockedMachine)

end CandySuite
