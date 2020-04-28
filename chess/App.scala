package chess

import zio.console._

object App extends zio.App {

  def run(args: List[String]) =
    logic.fold(e => 1, r => 0)

  def logic = for {
    _ <- putStrLn("App!")
    n <- getStrLn
  } yield n
}

