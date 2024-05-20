package state

import chess.*

class ChessSuite extends munit.FunSuite:
  val initialBoard = Board(List.empty, Player.White, 0)

  test("White's turn to play with Pa3 move"):
    val action = Action(Player.White, Piece.P, HPos._a, VPos._3)
    val (_, nextBoard) = Game
      .action(List(action))
      .run(initialBoard)

    assertEquals(nextBoard.turnToMove, Player.Black)
    assertEquals(nextBoard.move, initialBoard.move + 1)
    assertEquals(nextBoard.actions, initialBoard.actions :+ action)

  test(
    "White's turn to play with Pa3 move then Black's turn to play with Nh6"
  ):
    val action1 = Action(Player.White, Piece.P, HPos._a, VPos._3)
    val action2 = Action(Player.Black, Piece.N, HPos._h, VPos._6)

    val (_, nextBoard1) = Game
      .action(List(action1))
      .run(initialBoard)

    val (_, nextBoard2) = Game
      .action(List(action2))
      .run(nextBoard1)

    assertEquals(nextBoard2.turnToMove, Player.White)
    assertEquals(nextBoard2.move, initialBoard.move + 2)
    assertEquals(
      nextBoard2.actions,
      initialBoard.actions ::: List(action1, action2)
    )

end ChessSuite
