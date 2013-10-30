package chess

import language.implicitConversions

object Rank {

  /**
    * Returns the rank of a figure of the given type.
    */
  def figureRank(figure: Figure) = figure.figureType match {
    case Queen => 900
    case Rook => 450
    case Knight | Bishop => 300
    case Pawn => 100
    case _ => 0
  }

  /**
    * Returns the rank of the given field.
    */
  def fieldRank(field: Field) = {
    def colRowRank(cr: Int) = if (cr>=5) 9-cr else cr
    2*colRowRank(field.col) * colRowRank(field.row)
  }

  /**
    * Implicit conversion: Game -> Rank
    */
  implicit def Game2Rank(game: Game): Rank = new Rank(game)

}

class Rank(game: Game) {
  import Rank._
  import FigureMoves._

  /**
    * Returns the figure rank based on the figures it is defending.
    */
  def figureDefendingOtherFiguresRank(field:Field, figure:Figure) =
    game.defendedDestinations(figureMoves(figure,field,true)).size/2

  /**
    * Returns a rank value related to whether the King is under check or not.
    */
  def checkRank(color: Color) =
    if (game.color == color.other && game.isKingUnderCheck) 50
    else 0

  /**
    * Calculates the position rank taking one color into account.
    */
  def colorRank(color: Color) =
    (for ((field, figure) <- game.board.iterator
      if figure.figureColor == color;
      r1 = figureRank(figure);
      r2 = fieldRank(field);
      r3 = game.figureDefendingOtherFiguresRank(field, figure))
    yield r1 + r2 + r3).sum + game.checkRank(color)

  /**
    * Calculates the position rank from the point of view of a player.
    */
  def rank(color: Color) =
    game.colorRank(color)-game.colorRank(color.other)

}

/*
scaladoc chess\*.scala
scalac chess\*.scala
scala
import chess._, Rank._
figureRank(Figure(Queen,White)) // 900
figureRank(Figure(Knight,Black)) // 300
fieldRank(Field(1,1)) // 2
fieldRank(Field(2,5)) // 16
fieldRank(Field(4,4)) // 32
val g1 = GameStart.move(Field(1,2),Field(1,3),None).get
val g2 = g1.move(Field(1,7),Field(1,6),None).get
figureDefendingOtherFiguresRank(g2,Field(2,1),Figure(Knight,White)) // 1
val g1 = GameStart.move(Field(7,2),Field(7,4),None).get
val g2 = g1.move(Field(5,7),Field(5,6),None).get
val g3 = g2.move(Field(6,2),Field(6,4),None).get
val g4 = g3.move(Field(4,8),Field(8,4),None).get
checkRank(GameStart,White) // 0
checkRank(g4,White) // 0
checkRank(g4,Black) // 50
colorRank(GameStart,White) // 3928
colorRank(g1,White) // 3928
colorRank(g2,White) // 3935
colorRank(g3,White) // 3940
colorRank(g4,White) // 3947
rank(GameStart,White) // 8
rank(g1,White) // 0
rank(g2,White) // 7
rank(g3,White) // 5
rank(g4,White) // -32
rank(GameStart,Black) // -8
:q
*/
