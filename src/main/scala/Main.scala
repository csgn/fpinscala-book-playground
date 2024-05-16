import state.{Input, Machine, Candy}

@main def main(): Unit =
  val machine = Machine(true, 5, 10)
  val inputs =
    List(
      Input.Coin,
      Input.Turn,
      Input.Coin,
      Input.Turn,
      Input.Coin,
      Input.Turn,
      Input.Coin,
      Input.Turn
    )
  val s = Candy.simulateMachine(inputs).run(machine)
  println(s)

  // s = State(Machine(Locked=1, Candies=1, Coins=14), (Coins=14, Candies=1))
