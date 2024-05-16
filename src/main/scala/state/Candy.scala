package state

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for
      _ <- State.traverse(inputs)(i => State.modify(update(i)))
      s <- State.get
    yield (s.coins, s.candies)

  def update(i: Input)(s: Machine): Machine =
    (i, s) match
      // A machine that's out of candy ignores all inputs
      case (_, Machine(_, 0, _)) => s
      // Inserting a coin into an unlocked machine does nothing
      case (Input.Coin, Machine(false, _, _)) => s
      // Turning the knob on a locked machine does nothing
      case (Input.Turn, Machine(true, _, _)) => s
      // Inserting a coin into a locked machine will cause
      // to unlock if there's any candy left
      case (Input.Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      // Turning the knob on an unlocked machine will cause
      // it to dispense candy and become locked
      case (Input.Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
