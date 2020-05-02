package chess

import collection.immutable._

/**
  * Valid fields have coordinates in the range between 1 and 8.
  */
case class Field(col: Int, row: Int) {

  /**
    * Shows field coordinates as a pair of characters:
    * a letter representing the column and a number representing the row.
    */
  override def toString = (col + 'a' - 1).toChar.toString + row

  /**
    * Returns a new field with coordinates moved
    * by the given number of rows and columns relative to the original field.
    */
  def relative(c: Int, r: Int) = Field(col+c, row+r)

  /**
    * Returns a boolean value indicating
    * whether the given field belongs to the last row from
    * the point of view of a player.
    */
  def isLastRow(color: Color) = row == color.other.firstRow

  /**
    * Returns a boolean value indicating
    * whether the field has valid coordinates, that is
    * whether it belongs to the board.
    */
  def isValid = col >= 1 && col <= 8 && row >= 1 && row <= 8

}

/*
scaladoc chess\*.scala
scalac chess\*.scala
scala
import chess._
Field(1,1) // "a1"
Field(2,1) // "b1"
Field(2,3) // "b3"
Field(8,8) // "h8"
Field(2,3).relative(1,1) // "c4"
Field(2,3).relative(4,5) // "f8"
Field(8,8).isLastRow(White) // true
Field(7,8).isLastRow(White) // true
Field(7,8).isLastRow(Black) // false
Field(7,1).isLastRow(Black) // true
Field(2,2).isLastRow(White) // false
Field(2,2).isValid // true
Field(0,2).isValid // false
Field(2,0).isValid // false
Field(2,9).isValid // false
Field(9,2).isValid // false
:q
*/
