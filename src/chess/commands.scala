package chess

abstract sealed trait Command
case object NewC extends Command
case object HelpC extends Command
case object ShowC extends Command
case class LoadC(file: String) extends Command
case class SaveC(file: String) extends Command
case class MoveC(from: Field, to: Field, promotion: Option[Figure])

object Command {

  private final val LoadRe = "load (\\w+)".r

  def fromString(str: String): Option[Command] =
    str match {
      case "new" => Some(NewC)
      case "help" => Some(HelpC)
      case "show" => Some(ShowC)
      case LoadRe(file) => Some(LoadC(file))
      case _ => None
    }
}
