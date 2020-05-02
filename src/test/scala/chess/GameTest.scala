package chess


import zio.test._
import Assertion._
import FigureMoves._

object GameTest extends DefaultRunnableSpec {

  val g1 = GameStart.move(Field(7,2),Field(7,4),None).get
  val g2 = g1.move(Field(5,7),Field(5,6),None).get
  val g3 = g2.move(Field(6,2),Field(6,4),None).get
  val g4 = g3.move(Field(4,8),Field(8,4),None).get

  def spec = suite("game test")(
    test("all") {
      assert(GameStart.toString)(equalTo("""White to begin:
                                           | abcdefgh
                                           |8♜♞♝♛♚♝♞♜8
                                           |7♟♟♟♟♟♟♟♟7
                                           |6........6
                                           |5........5
                                           |4........4
                                           |3........3
                                           |2♙♙♙♙♙♙♙♙2
                                           |1♖♘♗♕♔♗♘♖1
                                           | abcdefgh""".stripMargin)) &&
      assert(GameStart.isFieldEmpty(Field(2,2)))(equalTo(false)) &&
      assert(GameStart.isFieldEmpty(Field(2,3)))(equalTo(true)) &&
      assert(GameStart.freeDestinations(figureMoves(Figure(Rook,White),Field(3,4),false)).map(_.toString))(equalTo(List("d4", "e4", "f4", "g4", "h4", "b4", "a4", "c5", "c6", "c3"))) &&
      assert(GameStart.freeDestinations(figureMoves(Figure(Bishop,White),Field(3,4),false)).map(_.toString))(equalTo(List("d5", "e6", "b5", "a6", "d3", "b3"))) &&
      assert(GameStart.captureDestinations(figureMoves(Figure(Rook,White),Field(3,4),false)).map(_.toString))(equalTo(List("c7"))) &&
      assert(GameStart.captureDestinations(figureMoves(Figure(Bishop,White),Field(3,4),false)).map(_.toString))(equalTo(List("f7"))) &&
      assert(GameStart.isOtherKingUnderCheck)(equalTo(false)) &&
      assert(GameStart.isKingUnderCheck)(equalTo(false)) &&
      assert(GameStart.isGameFinished)(equalTo(false)) &&
      assert(GameStart.winner)(equalTo(None)) &&
      assert(GameStart.nextGames.size)(equalTo(20)) &&
      assert(GameStart.validGames.size)(equalTo(20)) &&
      assert(GameStart.move(Field(1,2),Field(1,5),None))(equalTo(None)) &&
      assert(g4.isOtherKingUnderCheck)(equalTo(false)) &&
      assert(g4.isKingUnderCheck)(equalTo(true)) &&
      assert(g4.isGameFinished)(equalTo(true)) &&
      assert(g4.winner)(equalTo(Some(Black))) &&
      assert(g4.nextGames.size)(equalTo(20)) &&
      assert(g4.validGames.size)(equalTo(0))
    }
  )
}

