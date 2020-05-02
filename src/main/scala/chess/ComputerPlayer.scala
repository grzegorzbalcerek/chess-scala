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
