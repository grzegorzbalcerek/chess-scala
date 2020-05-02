package chess

import zio.test._

object AllTests extends DefaultRunnableSpec {
  override def spec =
    suite("all")(
      ColorTest.spec,
      BoardTest.spec,
      FieldTest.spec,
      FigureTest.spec,
      FigureMovesTest.spec,
      GameTest.spec,
      RankTest.spec,
      ComputerPlayerTest.spec)
}
