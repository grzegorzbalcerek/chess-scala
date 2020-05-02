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

