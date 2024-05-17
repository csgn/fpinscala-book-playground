package state

import state.Stack

class StackSuite extends munit.FunSuite:
  val initialStack: Stack[Int] = Stack(List.empty, 10, 10, None)
  val noCapStack: Stack[Int] = Stack(List(1, 2, 3), 3, 0, None)

  test("Stack: push an element to stack"):
    val (remainCap, stackNext) =
      Stack.action(Action.Push, Some(1)).run(initialStack)
    assertEquals(remainCap, initialStack.initialCap - 1)
    assertEquals(
      stackNext,
      Stack(List(1), initialStack.initialCap, 9, Some(Action.Push, 1))
    )

  test("Stack: pop an element from stack"):
    val (remainCap, stackNext) =
      Stack.action(Action.Push, Some(1)).run(initialStack)

    val (remainCap2, stackNext2) =
      Stack.action(Action.Push, Some(2)).run(stackNext)

    val (remainCap3, stackNext3) =
      Stack.action(Action.Pop, None).run(stackNext2)

    assertEquals(remainCap3, stackNext.remainCap)
    assertEquals(
      stackNext3,
      Stack(
        stackNext.elems,
        initialStack.initialCap,
        stackNext.remainCap,
        Some(Action.Pop, 2)
      )
    )

  test("Stack: push an element to zero capacity stack"):
    val (remainCap, stackNext) =
      Stack.action(Action.Push, Some(1)).run(noCapStack)

    assertEquals(remainCap, noCapStack.remainCap)
    assertEquals(stackNext, noCapStack)

  test("Stack: pop an element from zero capacity stack"):
    val (remainCap, stackNext) =
      Stack.action(Action.Push, None).run(noCapStack)

    assertEquals(remainCap, noCapStack.remainCap)
    assertEquals(stackNext, noCapStack)

end StackSuite
