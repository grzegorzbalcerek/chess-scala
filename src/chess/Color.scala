package chess

/**
  * The `Color` class represents one of the two colors (`Black` or `White`)
  * used in the game of Chess.
  */
abstract sealed class Color {

  /**
    * The `other` method returns the opposite color.
    */
  def other: Color

  /**
    * The `firstRow` method returns the coordinate of the first row
    * from the point of view of a player who plays the given color.
    */
  def firstRow: Int

}

case object White extends Color {
  def other = Black
  def firstRow = 1
}

case object Black extends Color {
  def other = White
  def firstRow = 8
}

/*
scaladoc chess\*.scala
scalac chess\*.scala
scala
import chess._
White.other // Black
Black.other // White
White.firstRow // 1
White.firstRow // 2
:q
*/
