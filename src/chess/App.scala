package chess

import zio.console._

object App extends zio.App {

  def run(args: List[String]) =
    logic

  def logic = for {
    _ <- putStrLn("App!")
  } yield 1
}

