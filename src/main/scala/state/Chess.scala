package chess

import state.State

enum Piece(val name: String):
  case P extends Piece("Pawn")
  case R extends Piece("Rook")
  case N extends Piece("Knight")
  case B extends Piece("Bishop")
  case Q extends Piece("Queen")
  case K extends Piece("King")

enum VPos(val name: Char):
  case _1 extends VPos('1')
  case _2 extends VPos('2')
  case _3 extends VPos('3')
  case _4 extends VPos('4')
  case _5 extends VPos('5')
  case _6 extends VPos('6')
  case _7 extends VPos('7')
  case _8 extends VPos('8')

enum HPos(val name: Char):
  case _a extends HPos('a')
  case _b extends HPos('b')
  case _c extends HPos('c')
  case _d extends HPos('d')
  case _e extends HPos('e')
  case _f extends HPos('f')
  case _g extends HPos('g')
  case _h extends HPos('h')

enum Player:
  case White, Black

case class Action(
    player: Player,
    piece: Piece,
    hpos: HPos,
    vpos: VPos
)

case class Board(actions: List[Action], turnToMove: Player, move: Int)

object Game:
  def action(actions: List[Action]): State[Board, Int] =
    for
      _ <- State.traverse(actions)(action => State.modify(update(action)))
      s <- State.get
    yield s.move

  private def update(a: Action)(b: Board): Board =
    (a, b) match
      case (_, _) if a.player != b.turnToMove => b
      case (_, _) if a.player == b.turnToMove =>
        // TODO check illegal move
        val nextActions = b.actions :+ a
        val nextPlayer =
          if a.player == Player.White then Player.Black else Player.White
        val nextMove = b.move + 1
        Board(
          nextActions,
          nextPlayer,
          nextMove
        )
      case _ => b
