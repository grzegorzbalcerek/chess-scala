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
