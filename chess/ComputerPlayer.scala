package chess

import language.implicitConversions

object ComputerPlayer {

  /**
    * Randomly chooses one of the elements of the given sequence.
    */
  def chooseRandomly(moves: Seq[Game]) = {
    import scala.util.Random
    if (moves.isEmpty) None
    else Some(moves(Random.nextInt(moves.size)))
  }

  /**
    * Implicit conversion: Game -> ComputerPlayer
    */
  implicit def Game2ComputerPlayer(game: Game): ComputerPlayer =
    new ComputerPlayer(game)

}

class ComputerPlayer(game: Game) {
  import ComputerPlayer._
  import Rank._

  /**
    * Returns a sequence of the best ranked moves.
    */
  def moves: Seq[Game] = {
    val moves = game.validGames.toList
    if (moves.isEmpty) Seq()
    else {
      val rankedMoves = moves.map(g => (g, g.rank(game.color)))
      val rankedMovesSorted = rankedMoves.sortBy(- _._2)
      val firstRank = rankedMovesSorted.head._2
      rankedMovesSorted.takeWhile(_._2 == firstRank).map(_._1)
    }
  }

  /**
    * Makes a move and returns the next game state.
    */
  def makeMove = chooseRandomly(moves)

}

/*
scaladoc chess\*.scala
scalac chess\*.scala
scala
import chess._, ComputerPlayer._
val g1 = GameStart.move(Field(7,2),Field(7,4),None).get
val g2 = g1.move(Field(5,7),Field(5,6),None).get
val g3 = g2.move(Field(6,2),Field(6,4),None).get
val g4 = g3.move(Field(4,8),Field(8,4),None).get
GameStart.moves.size // 2
g1.moves.size // 2
g2.moves.size // 2
g3.moves.size // 1
g4.moves.size // 0
val g1 = GameStart.makeMove.get
val g2 = g1.makeMove.get
val g3 = g2.makeMove.get
val g4 = g3.makeMove.get
:q
*/
