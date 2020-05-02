package chess


import zio.test._
import Assertion._

object FigureTest extends DefaultRunnableSpec {

  def spec = suite("figure test")(
    test("toString"){
      assert(Figure(King, White).toString)(equalTo("k")) &&
      assert(Figure(King, Black).toString)(equalTo("K")) &&
      assert(Figure(Knight, White).toString)(equalTo("n"))
    },
    test("figureSymbol"){
      assert(Figure(King, White).figureSymbol)(equalTo("♔")) &&
      assert(Figure(King, Black).figureSymbol)(equalTo("♚")) &&
      assert(Figure(Knight, White).figureSymbol)(equalTo("♘"))
    }
  )
}

