package chess


import zio.test._
import Assertion._

import Board._

object BoardTest extends DefaultRunnableSpec {

  def spec = suite("board test")(
    test("showBoardA"){
      assert(showBoardA(startingBoard))(equalTo(""" abcdefgh
                                                  |8RNBQKBNR8
                                                  |7PPPPPPPP7
                                                  |6........6
                                                  |5........5
                                                  |4........4
                                                  |3........3
                                                  |2pppppppp2
                                                  |1rnbqkbnr1
                                                  | abcdefgh""".stripMargin))
    },
    test("showBoardU"){
      assert(showBoardU(startingBoard))(equalTo(""" abcdefgh
                                                  |8♜♞♝♛♚♝♞♜8
                                                  |7♟♟♟♟♟♟♟♟7
                                                  |6........6
                                                  |5........5
                                                  |4........4
                                                  |3........3
                                                  |2♙♙♙♙♙♙♙♙2
                                                  |1♖♘♗♕♔♗♘♖1
                                                  | abcdefgh""".stripMargin))
    },
    test("updateBoard"){
      assert(showBoardA(updateBoard(startingBoard,RegularMove(Field(2,2),Field(2,3)))))(
        equalTo(""" abcdefgh
                   |8RNBQKBNR8
                   |7PPPPPPPP7
                   |6........6
                   |5........5
                   |4........4
                   |3.p......3
                   |2p.pppppp2
                   |1rnbqkbnr1
                   | abcdefgh""".stripMargin)) &&
      assert(showBoardA(updateBoard(startingBoard,RegularMove(Field(3,2),Field(2,3)))))(
        equalTo(""" abcdefgh
                   |8RNBQKBNR8
                   |7PPPPPPPP7
                   |6........6
                   |5........5
                   |4........4
                   |3.p......3
                   |2pp.ppppp2
                   |1rnbqkbnr1
                   | abcdefgh""".stripMargin)) &&
      assert(showBoardA(updateBoard(startingBoard,PromotionMove(Field(2,2),Field(2,8),Figure(Queen,White)))))(
        equalTo(""" abcdefgh
                   |8RqBQKBNR8
                   |7PPPPPPPP7
                   |6........6
                   |5........5
                   |4........4
                   |3........3
                   |2p.pppppp2
                   |1rnbqkbnr1
                   | abcdefgh""".stripMargin)) &&
      assert(showBoardA(updateBoard(startingBoard,EnPassantMove(Field(2,2),Field(3,3),Field(3,7)))))(
        equalTo(""" abcdefgh
                   |8RNBQKBNR8
                   |7PP.PPPPP7
                   |6........6
                   |5........5
                   |4........4
                   |3..p.....3
                   |2p.pppppp2
                   |1rnbqkbnr1
                   | abcdefgh""".stripMargin))
      assert(showBoardA(updateBoard(startingBoard,CastlingMove(Field(5,1),Field(3,1),Field(1,1),Field(4,1)))))(
        equalTo(""" abcdefgh
                  |8RNBQKBNR8
                  |7PPPPPPPP7
                  |6........6
                  |5........5
                  |4........4
                  |3........3
                  |2pppppppp2
                  |1.nkr.bnr1
                  | abcdefgh""".stripMargin))
    }
  )
}
