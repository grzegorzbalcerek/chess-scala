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
