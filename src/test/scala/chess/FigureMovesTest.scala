package chess


import zio.test._
import Assertion._
import FigureMoves._

object FigureMovesTest extends DefaultRunnableSpec {

  def spec = suite("figuremoves test")(
    test("all") {
      assert(rookMoves.map { case (a, b) => (a.take(4).force, b.take(4).force) })(equalTo(List((LazyList(1, 2, 3, 4), LazyList(0, 0, 0, 0)), (LazyList(-1, -2, -3, -4), LazyList(0, 0, 0, 0)), (LazyList(0, 0, 0, 0), LazyList(1, 2, 3, 4)), (LazyList(0, 0, 0, 0), LazyList(-1, -2, -3, -4))))) &&
        assert(bishopMoves.map { case (a, b) => (a.take(4).force, b.take(4).force) })(equalTo(List((LazyList(1, 2, 3, 4), LazyList(1, 2, 3, 4)), (LazyList(-1, -2, -3, -4), LazyList(1, 2, 3, 4)), (LazyList(1, 2, 3, 4), LazyList(-1, -2, -3, -4)), (LazyList(-1, -2, -3, -4), LazyList(-1, -2, -3, -4))))) &&
        assert(queenMoves.map { case (a, b) => (a.take(4).force, b.take(4).force) })(equalTo(List((LazyList(1, 2, 3, 4), LazyList(0, 0, 0, 0)), (LazyList(-1, -2, -3, -4), LazyList(0, 0, 0, 0)), (LazyList(0, 0, 0, 0), LazyList(1, 2, 3, 4)), (LazyList(0, 0, 0, 0), LazyList(-1, -2, -3, -4)), (LazyList(1, 2, 3, 4), LazyList(1, 2, 3, 4)), (LazyList(-1, -2, -3, -4), LazyList(1, 2, 3, 4)), (LazyList(1, 2, 3, 4), LazyList(-1, -2, -3, -4)), (LazyList(-1, -2, -3, -4), LazyList(-1, -2, -3, -4))))) &&
        assert(knightMoves.map { case (a, b) => (a.force, b.force) })(equalTo(List((LazyList(1), LazyList(2)), (LazyList(2), LazyList(1)), (LazyList(-1), LazyList(2)), (LazyList(2), LazyList(-1)), (LazyList(-1), LazyList(-2)), (LazyList(-2), LazyList(-1)), (LazyList(1), LazyList(-2)), (LazyList(-2), LazyList(1))))) &&
        assert(kingMoves.map { case (a, b) => (a.force, b.force) })(equalTo(List((LazyList(1), LazyList(0)), (LazyList(-1), LazyList(0)), (LazyList(0), LazyList(1)), (LazyList(0), LazyList(-1)), (LazyList(1), LazyList(1)), (LazyList(-1), LazyList(1)), (LazyList(1), LazyList(-1)), (LazyList(-1), LazyList(-1))))) &&
        assert(chooseFigureMoves(Figure(Pawn, White), Field(4, 2), false).map { case (a, b) => (a.take(4).force, b.take(4).force) })(equalTo(List((LazyList(0, 0, 0, 0), LazyList(1, 2))))) &&
        assert(chooseFigureMoves(Figure(Pawn, White), Field(4, 4), false).map { case (a, b) => (a.take(4).force, b.take(4).force) })(equalTo(List((LazyList(0), LazyList(1))))) &&
        assert(chooseFigureMoves(Figure(Pawn, Black), Field(4, 7), false).map { case (a, b) => (a.take(4).force, b.take(4).force) })(equalTo(List((LazyList(0, 0, 0, 0), LazyList(-1, -2))))) &&
        assert(chooseFigureMoves(Figure(Pawn, Black), Field(4, 5), false).map { case (a, b) => (a.take(4).force, b.take(4).force) })(equalTo(List((LazyList(0), LazyList(-1))))) &&
        assert(chooseFigureMoves(Figure(Pawn, White), Field(4, 2), true).map { case (a, b) => (a.take(4).force, b.take(4).force) })(equalTo(List((LazyList(-1), LazyList(1)), (LazyList(1), LazyList(1))))) &&
        assert(chooseFigureMoves(Figure(Pawn, Black), Field(4, 7), true).map { case (a, b) => (a.take(4).force, b.take(4).force) })(equalTo(List((LazyList(-1), LazyList(-1)), (LazyList(1), LazyList(-1))))) &&
        assert(relativeField(Field(1, 2))((1, 1)).toString)(equalTo("b3")) &&
        assert(relativeField(Field(1, 2))((0, 2)).toString)(equalTo("a4")) &&
        assert(relativeFields(Field(2, 2))((LazyList(0, 0), LazyList(1, 2))).force.map(_.toString))(equalTo(LazyList("b3", "b4"))) &&
        assert(figureMoves(Figure(Rook, White), Field(3, 4), false).map(_.force.map(_.toString)))(equalTo(List(LazyList("d4", "e4", "f4", "g4", "h4"), LazyList("b4", "a4"), LazyList("c5", "c6", "c7", "c8"), LazyList("c3", "c2", "c1")))) &&
        assert(figureMoves(Figure(Pawn, White), Field(2, 2), false).map(_.force.map(_.toString)))(equalTo(List(LazyList("b3", "b4")))) &&
        assert(figureMoves(Figure(Pawn, White), Field(2, 2), true).map(_.force.map(_.toString)))(equalTo(List(LazyList("a3"), LazyList("c3")))) &&
        assert(figureMoves(Figure(Pawn, White), Field(1, 2), true).map(_.force.map(_.toString)))(equalTo(List(LazyList(), LazyList("b3"))))
    }
  )
}

