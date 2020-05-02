package chess

import java.lang.System.lineSeparator

object Board {
  type Board = Map[Field,Figure]

  /**
    * The board state when the game starts.
    */
  def startingBoard: Board = Map(
    Field(1,1) -> Figure(Rook,White),
    Field(2,1) -> Figure(Knight,White),
    Field(3,1) -> Figure(Bishop,White),
    Field(4,1) -> Figure(Queen,White),
    Field(5,1) -> Figure(King,White),
    Field(6,1) -> Figure(Bishop,White),
    Field(7,1) -> Figure(Knight,White),
    Field(8,1) -> Figure(Rook,White),
    Field(1,2) -> Figure(Pawn,White),
    Field(2,2) -> Figure(Pawn,White),
    Field(3,2) -> Figure(Pawn,White),
    Field(4,2) -> Figure(Pawn,White),
    Field(5,2) -> Figure(Pawn,White),
    Field(6,2) -> Figure(Pawn,White),
    Field(7,2) -> Figure(Pawn,White),
    Field(8,2) -> Figure(Pawn,White),
    Field(1,7) -> Figure(Pawn,Black),
    Field(2,7) -> Figure(Pawn,Black),
    Field(3,7) -> Figure(Pawn,Black),
    Field(4,7) -> Figure(Pawn,Black),
    Field(5,7) -> Figure(Pawn,Black),
    Field(6,7) -> Figure(Pawn,Black),
    Field(7,7) -> Figure(Pawn,Black),
    Field(8,7) -> Figure(Pawn,Black),
    Field(1,8) -> Figure(Rook,Black),
    Field(2,8) -> Figure(Knight,Black),
    Field(3,8) -> Figure(Bishop,Black),
    Field(4,8) -> Figure(Queen,Black),
    Field(5,8) -> Figure(King,Black),
    Field(6,8) -> Figure(Bishop,Black),
    Field(7,8) -> Figure(Knight,Black),
    Field(8,8) -> Figure(Rook,Black))

  val showBoardU = showBoard(_.figureSymbol)_
  val showBoardA = showBoard(_.toString)_

  /**
   * Shows the board.
   */
  private def showBoard(show: Figure => String)(board: Board): String = {
    def rowToString(row: Int) = 1.to(8).map(col=>
      board.get(Field(col,row)).map(show).getOrElse(".")).mkString
    " abcdefgh" + lineSeparator + 8.to(1,-1).map(row =>
    row.toString + rowToString(row) + row.toString + lineSeparator).mkString + " abcdefgh"
  }

  /**
    * Returns a new board, updated with a move.
    */
  def updateBoard(board: Board, move: Move): Board = move match {
    case RegularMove(from,to) =>
      board.get(from).fold(board)( figure =>
        board - from + (to->figure))
    case PromotionMove(from,to,figure) =>
      board.get(from).fold(board)( _ =>
        board - from + (to->figure))
    case EnPassantMove(from,to,captured) =>
      board.get(from).fold(board)( figure =>
        board - from - captured + (to->figure))
    case CastlingMove(from,to,rookFrom,rookTo) =>
      (board.get(from), board.get(rookFrom)) match {
        case (Some(figure),Some(rookFigure)) =>
          board - from + (to->figure) - rookFrom + (rookTo->rookFigure)
        case _ => board
      }
  }
}
