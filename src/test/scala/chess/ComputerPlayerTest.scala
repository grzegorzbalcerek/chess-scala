package chess


import zio.test._
import Assertion._
import ComputerPlayer._

object ComputerPlayerTest extends DefaultRunnableSpec {

  val g1 = GameStart.move(Field(7,2),Field(7,4),None).get
  val g2 = g1.move(Field(5,7),Field(5,6),None).get
  val g3 = g2.move(Field(6,2),Field(6,4),None).get
  val g4 = g3.move(Field(4,8),Field(8,4),None).get

  def spec = suite("computer player test")(
    test("all") {
      assert(GameStart.moves.size)(equalTo(2)) &&
        assert(g1.moves.size)(equalTo(2)) &&
        assert(g2.moves.size)(equalTo(2)) &&
        assert(g3.moves.size)(equalTo(1)) &&
        assert(g4.moves.size)(equalTo(0))
    }
  )

}

