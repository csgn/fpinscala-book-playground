package state

enum Action:
  case Push, Pop

case class Stack[A](
    elems: List[A],
    initialCap: Int,
    remainCap: Int,
    lastAction: Option[(Action, A)]
)

object Stack:
  def action[A](action: Action, dat: Option[A]): State[Stack[A], Int] =
    for
      _ <- State.modify(update(action, dat))
      s <- State.get
    yield s.remainCap

  private def update[A](action: Action, dat: Option[A])(s: Stack[A]): Stack[A] =
    (action, s, dat) match
      case (Action.Push, Stack(_, _, 0, _), _) => s
      case (Action.Pop, Stack(_, _, 0, _), _)  => s
      case (Action.Push, _, Some(d)) =>
        Stack(
          s.elems ::: List(d),
          s.initialCap,
          s.remainCap - 1,
          Some(Action.Push, d)
        )
      case (Action.Pop, _, None) =>
        Stack(
          s.elems.init,
          s.initialCap,
          s.remainCap + 1,
          Some(Action.Pop, s.elems.last)
        )
      case _ => s
