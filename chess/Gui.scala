package chess

import swing._, actors.Actor, java.awt.{Rectangle,Font}

/**
  * Implements the window content.
  */
class GamePanel(playerColor: Color) extends Panel {
  import ComputerPlayer._

  /**
    * Stores the current game state.
    */
  private[this] var game =
    if (playerColor == White) GameStart else GameStart.makeMove.get

  /**
    * Stores the information whether and what field on the board
    * has been selected by the player.
    */
  private[this] var selectedField: Option[Field] = None

  preferredSize = new Dimension(450,450)

  /**
    * Calculates the size of a single board field.
    */
  def fieldSize = (size.width min size.height)/9

  // notify the panel object about the mouse click events
  listenTo(mouse.clicks)

  // implement the reactions to user actions
  reactions += {
    case FieldClicked(field) =>
      if (game.color == playerColor) {
        selectedField match {
          case None => selectedField = Some(field); repaint
          case Some(from) =>
            selectedField = None
            val newGame = game.move(from, field, promotionFigure(from, field))
            if (newGame != None) {
              game = newGame.get
              repaint
              if (game.isGameFinished) gameOver
              else Actor.actor {
                val newGame = game.makeMove
                if (newGame == None) gameOver
                else Swing.onEDT { game = newGame.get; repaint}
              }
            }
        }
      }
  }

  /**
    * An extractor, which transforms the mouse click data into
    * information about the field board that has been clicked.
    */
  object FieldClicked {
    import event._
    def unapply(bc: MouseClicked): Option[Field] = {
      val fs = fieldSize
      val col = (bc.point.x + fs/2) / fs
      val row = 9 - (bc.point.y + fs/2) / fs
      val field = new Field(col, row)
      if (field.isValid) Some(field) else None
    }
  }

  /**
    * Verifies if there are any Pawn moves which require a promotion
    * in the set of valid moves. If that is the case and there are
    * at least two different figures possible to be chosen, a window
    * is presented to the user and the user can choose the figure.
    */
  def promotionFigure(from: Field, to:Field): Option[Figure] = {
    val promotionFigures = game.validGames.map(_.lastMove).
      filter{
        case PromotionMove(f,t,_) => f == from && t == to
        case _ => false
      }.map(_.asInstanceOf[PromotionMove].figure).toSet
    promotionFigures.size match {
      case 0 => None
      case 1 => Some(promotionFigures.head)
      case _ =>
        Dialog.showInput[Figure](parent = this,
          message = "Choose the figure to promote the Pawn to",
          entries = promotionFigures.toList,
          initial = promotionFigures.head)
    }
  }

  /**
    * This method is called when the game is over.
    * It stops the mouse handling and shows a messages with
    * the information about the game end and the result.
    */
  def gameOver {
    deafTo(mouse.clicks)
    Dialog.showMessage(parent = this, message = "Game over. " +
      (game.winner match {
        case Some(winnerColor) => "Winner: "+winnerColor+"."
        case None => "No winner."
      }))
  }

  /**
    * Draws the panel content.
    */
  override def paintComponent(g: Graphics2D) {
    super.paintComponent(g)
    val fs = fieldSize
    drawFields
    drawBoardBorder
    drawCoordinates
    drawFigures
    drawSelectedField
    // Draws the board fields using the yellow and red colors.
    def drawFields {
      import java.awt.Color._
      for (col <- 0 to 7; row <- 0 to 7) {
        g.setPaint(if((col+row)%2==0) YELLOW else RED)
        g.fill(new Rectangle(fs/2+col*fs,
         fs/2+row*fs, fs, fs))
      }
    }
    // Draws the black board border.
    def drawBoardBorder {
      g.setPaint(java.awt.Color.BLACK)
      g.draw(new Rectangle(fs/2,fs/2,8*fs,8*fs))
    }
    // Draws the board coordinates, that is the row numbers and
    // the letters denoting the columns.
    def drawCoordinates {
      g.setFont(new Font(g.getFont.getName, g.getFont.getStyle, fs*2/5))
      val rect = g.getFont.getStringBounds("0",
        g.getFontRenderContext)
      val xOffset = (rect.getX+rect.getWidth/2).toInt
      val yOffset = (rect.getY+rect.getHeight/2).toInt
      for (j <- 1 to 8; row = 9-j; col = ('a'-1+j).toChar) {
        g.drawString(row.toString,fs/4-xOffset,j*fs-yOffset)
        g.drawString(row.toString,fs/4+fs/2+8*fs-xOffset, j*fs-yOffset)
        g.drawString(col.toString,j*fs-xOffset,fs/4-yOffset)
        g.drawString(col.toString,j*fs-xOffset, fs/4+fs/2+fs*8-yOffset)
      }
    }
    // Draws the figures on the board.
    def drawFigures {
      g.setFont(new Font(g.getFont.getName,g.getFont.getStyle,fs))
      val rect = g.getFont.getStringBounds(Figure(King,White).figureSymbol,
        g.getFontRenderContext)
      val xOffset = (rect.getX+rect.getWidth/2).toInt
      val yOffset = (rect.getY+rect.getHeight/2).toInt
      for ((field, figure) <- game.board.iterator)
        g.drawString(figure.figureSymbol,field.col*fs-xOffset,
                     (9-field.row)*fs-yOffset)
    }
    // Draws a blue border around the field selected by the player.
    def drawSelectedField {
      selectedField match {
        case Some(field) => 
          g.setPaint(java.awt.Color.BLUE)
          val fs = fieldSize
          g.draw(new Rectangle(-fs/2+field.col*fs,-fs/2+(9-field.row)*fs,fs,fs))
          g.draw(new Rectangle(-fs/2+field.col*fs+1,-fs/2+(9-field.row)*fs+1,
                               fs-2,fs-2))
        case None =>
      }
    }
  }

}

/**
  * Implements the main application window.
  */
class App(playerColor: Color) extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Chess game. Player: " + playerColor +
      ". Computer: " + playerColor.other + "."
    contents = new GamePanel(playerColor) }
}

/**
  * Starts the game.
  * The player plays white and the computer plays black figures.
  */
object PlayWhite extends App(White)

/**
  * Starts the game.
  * The player plays black and the computer plays white figures.
  */
object PlayBlack extends App(Black)

/*
scaladoc chess\*.scala
scalac chess\*.scala
scala chess.PlayWhite
scala chess.PlayBlack
*/
