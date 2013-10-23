package chess

abstract sealed class Move {
  val from: Field
  val to: Field
}
case class RegularMove(from: Field, to: Field) extends Move
case class PromotionMove(from: Field, to: Field, figure: Figure) extends Move
case class EnPassantMove(from: Field, to: Field, captured: Field) extends Move
case class CastlingMove(from: Field, to: Field, rookFrom: Field, rookTo: Field) extends Move

/*
scaladoc chess\*.scala
scalac chess\*.scala
*/

