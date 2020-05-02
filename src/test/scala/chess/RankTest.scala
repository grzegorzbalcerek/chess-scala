package chess


import zio.test._
import Assertion._
import Rank._

object RankTest extends DefaultRunnableSpec {
  val f1 = GameStart.move(Field(1,2),Field(1,3),None).get
  val f2 = f1.move(Field(1,7),Field(1,6),None).get

  val g1 = GameStart.move(Field(7,2),Field(7,4),None).get
  val g2 = g1.move(Field(5,7),Field(5,6),None).get
  val g3 = g2.move(Field(6,2),Field(6,4),None).get
  val g4 = g3.move(Field(4,8),Field(8,4),None).get

  def spec = suite("rank test")(
    test("all") {
      assert(figureRank(Figure(Queen,White)))(equalTo(900)) &&
        assert(figureRank(Figure(Knight,Black)))(equalTo(300)) &&
        assert(fieldRank(Field(1,1)))(equalTo(2)) &&
        assert(fieldRank(Field(2,5)))(equalTo(16)) &&
        assert(fieldRank(Field(4,4)))(equalTo(32)) &&
        assert(f2.figureDefendingOtherFiguresRank(Field(2,1),Figure(Knight,White)))(equalTo(1)) &&
        assert(GameStart.checkRank(White))(equalTo(0)) &&
        assert(g4.checkRank(White))(equalTo(0)) &&
        assert(g4.checkRank(Black))(equalTo(50)) &&
        assert(GameStart.colorRank(White))(equalTo(3928)) &&
        assert(g1.colorRank(White))(equalTo(3928)) &&
        assert(g2.colorRank(White))(equalTo(3935)) &&
        assert(g3.colorRank(White))(equalTo(3940)) &&
        assert(g4.colorRank(White))(equalTo(3947)) &&
        assert(GameStart.rank(White))(equalTo(8)) &&
        assert(g1.rank(White))(equalTo(0)) &&
        assert(g2.rank(White))(equalTo(7)) &&
        assert(g3.rank(White))(equalTo(5)) &&
        assert(g4.rank(White))(equalTo(-32)) &&
        assert(GameStart.rank(Black))(equalTo(-8))
    }
  )
}

