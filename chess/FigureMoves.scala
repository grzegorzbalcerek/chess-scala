package chess

import collection.immutable._

object FigureMoves {
  import LazyList._

  /**
    * Sequences of relative figure positions for rook moves.
    */
  def rookMoves: Seq[(LazyList[Int],LazyList[Int])] =
    Seq((from(1,1),continually(0)),
        (from(-1,-1),continually(0)),
        (continually(0),from(1,1)),
        (continually(0),from(-1,-1)))

  /**
    * Sequences of relative figure positions for bishop moves.
    */
  def bishopMoves: Seq[(LazyList[Int],LazyList[Int])] =
    Seq((from(1,1),from(1,1)),
        (from(-1,-1),from(1,1)),
        (from(1,1),from(-1,-1)),
        (from(-1,-1),from(-1,-1)))

  /**
    * Sequences of relative figure positions for queen moves.
    */
  def queenMoves: Seq[(LazyList[Int],LazyList[Int])] = rookMoves ++ bishopMoves

  /**
    * Sequences of relative figure positions for knight moves.
    */
  def knightMoves: Seq[(LazyList[Int],LazyList[Int])] =
    Seq((LazyList(1),LazyList(2)),
        (LazyList(2),LazyList(1)),
        (LazyList(-1),LazyList(2)),
        (LazyList(2),LazyList(-1)),
        (LazyList(-1),LazyList(-2)),
        (LazyList(-2),LazyList(-1)),
        (LazyList(1),LazyList(-2)),
        (LazyList(-2),LazyList(1)))

  /**
    * Sequences of relative figure positions for king moves.
    */
  def kingMoves: Seq[(LazyList[Int],LazyList[Int])] =
    queenMoves.map{case (a,b) => (a.take(1),b.take(1)) }

  /**
    * Choose the sequences of relative figure positions
    * based on the figure position, type, color,
    * and whether the move is a capture move or not.
    */
  def chooseFigureMoves(figure: Figure, field: Field, capture: Boolean): Seq[(LazyList[Int],LazyList[Int])] =
    figure.figureType match {
      case Rook => rookMoves
      case Bishop => bishopMoves
      case Queen => queenMoves
      case King => kingMoves
      case Knight => knightMoves
      case Pawn => capture match {
        case false => figure.figureColor match {
          case White => if (field.row == 2) Seq((continually(0),LazyList(1,2)))
                        else Seq((LazyList(0),LazyList(1)))
          case Black => if (field.row == 7) Seq((continually(0),LazyList(-1,-2)))
                        else Seq((LazyList(0),LazyList(-1))) }
        case true =>  figure.figureColor match {
          case White => Seq((LazyList(-1),LazyList(1)),(LazyList(1),LazyList(1)))
          case Black => Seq((LazyList(-1),LazyList(-1)),(LazyList(1),LazyList(-1))) } } }

  /**
    * Returns the field relative to the given field according to
    * a pair of relative coordinates.
    */
  def relativeField(field: Field)(cr: (Int,Int)): Field =
    Field(field.col+cr._1, field.row+cr._2)

  /**
    * Returns fields relative to the given field according to
    * the sequence of relative coordinates.
    */
  def relativeFields(field: Field)(colsRows: (LazyList[Int],LazyList[Int])): LazyList[Field] =
    colsRows._1.zip(colsRows._2).map(relativeField(field)).takeWhile(_.isValid)

  /**
    * Returns possible figure moves.
    * The figure is on the field 'field' and the 'capture' flag indicate whether
    * the move is a capture.
    */
  def figureMoves(figure: Figure, field: Field, capture: Boolean): Seq[LazyList[Field]] =
    chooseFigureMoves(figure, field, capture).map(relativeFields(field))

}

/*
scaladoc chess\*.scala
scalac chess\*.scala
scala
import chess._, FigureMoves._
rookMoves.map{case (a,b) => (a.take(4).force,b.take(4).force) } // List((LazyList(1, 2, 3, 4),LazyList(0, 0, 0, 0)), (LazyList(-1, -2, -3, -4),LazyList(0, 0, 0, 0)), (LazyList(0, 0, 0, 0),LazyList(1, 2, 3, 4)), (LazyList(0, 0, 0, 0),LazyList(-1, -2, -3, -4)))
bishopMoves.map{case (a,b) => (a.take(4).force,b.take(4).force) } // List((LazyList(1, 2, 3, 4),LazyList(1, 2, 3, 4)), (LazyList(-1, -2, -3, -4),LazyList(1, 2, 3, 4)), (LazyList(1, 2, 3, 4),LazyList(-1, -2, -3, -4)), (LazyList(-1, -2, -3, -4),LazyList(-1, -2, -3, -4)))
queenMoves.map{case (a,b) => (a.take(4).force,b.take(4).force) } // List((LazyList(1, 2, 3, 4),LazyList(0, 0, 0, 0)), (LazyList(-1, -2, -3, -4),LazyList(0, 0, 0, 0)), (LazyList(0, 0, 0, 0),LazyList(1, 2, 3, 4)), (LazyList(0, 0, 0, 0),LazyList(-1, -2, -3, -4)), (LazyList(1, 2, 3, 4),LazyList(1, 2, 3, 4)), (LazyList(-1, -2, -3, -4),LazyList(1, 2, 3, 4)), (LazyList(1, 2, 3, 4),LazyList(-1, -2, -3, -4)), (LazyList(-1, -2, -3, -4),LazyList(-1, -2, -3, -4)))
knightMoves.map{case (a,b) => (a.force,b.force) } // List((LazyList(1),LazyList(2)), (LazyList(2),LazyList(1)), (LazyList(-1),LazyList(2)), (LazyList(2),LazyList(-1)), (LazyList(-1),LazyList(-2)), (LazyList(-2),LazyList(-1)), (LazyList(1),LazyList(-2)), (LazyList(-2),LazyList(1)))
kingMoves.map{case (a,b) => (a.force,b.force) } // List((LazyList(1),LazyList(0)), (LazyList(-1),LazyList(0)), (LazyList(0),LazyList(1)), (LazyList(0),LazyList(-1)), (LazyList(1),LazyList(1)), (LazyList(-1),LazyList(1)), (LazyList(1),LazyList(-1)), (LazyList(-1),LazyList(-1)))
chooseFigureMoves(Figure(Pawn,White),Field(4,2),false).map{case (a,b) => (a.take(4).force,b.take(4).force) } // List((LazyList(0, 0, 0, 0),LazyList(1, 2)))
chooseFigureMoves(Figure(Pawn,White),Field(4,4),false).map{case (a,b) => (a.take(4).force,b.take(4).force) } // List((LazyList(0),LazyList(1)))
chooseFigureMoves(Figure(Pawn,Black),Field(4,7),false).map{case (a,b) => (a.take(4).force,b.take(4).force) } // List((LazyList(0, 0, 0, 0),LazyList(-1, -2)))
chooseFigureMoves(Figure(Pawn,Black),Field(4,5),false).map{case (a,b) => (a.take(4).force,b.take(4).force) } // List((LazyList(0),LazyList(-1)))
chooseFigureMoves(Figure(Pawn,White),Field(4,2),true).map{case (a,b) => (a.take(4).force,b.take(4).force) } // List((LazyList(-1),LazyList(1)), (LazyList(1),LazyList(1)))
chooseFigureMoves(Figure(Pawn,Black),Field(4,7),true).map{case (a,b) => (a.take(4).force,b.take(4).force) } // List((LazyList(-1),LazyList(-1)), (LazyList(1),LazyList(-1)))
relativeField(Field(1,2))((1,1)) // b3
relativeField(Field(1,2))((0,2)) // a4
relativeFields(Field(2,2))((LazyList(0,0),LazyList(1,2))).force // LazyList(b3, b4)
figureMoves(Figure(Rook,White),Field(3,4),false).map(_.force) // List(LazyList(d4, e4, f4, g4, h4), LazyList(b4, a4), LazyList(c5, c6, c7, c8), LazyList(c3, c2, c1))
figureMoves(Figure(Pawn,White),Field(2,2),false).map(_.force) // List(LazyList(b3, b4))
figureMoves(Figure(Pawn,White),Field(2,2),true).map(_.force) // List(LazyList(a3), LazyList(c3))
figureMoves(Figure(Pawn,White),Field(1,2),true).map(_.force) // List(LazyList(), LazyList(b3))
:q
*/
