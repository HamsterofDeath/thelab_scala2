package hod.other.stuff.boardgames.tictactoe

import hod.other.stuff.boardgames.logic.{AutoPlay, BoardPrinter, BoardRating, BoardState,
  GameContext, Move, MovesExhausted, MutableBoard, OutcomeNotDetermined, WinnerDetermined}

case class PlaceMarker(x: Int, y: Int, player: Int) extends Move
class TicTacToeBoard extends MutableBoard[PlaceMarker] {
  def isTurnOfMaximizingPlayer = currentTurnPlayer == 1

  def playerOn(x: Int, y: Int) = data(x)(y)

  def otherPlayer: Int =
    currentTurnPlayer match {
      case 1 => 2
      case 2 => 1
    }

  def rateSelfAsSeenFromPlayerOne(): Int = {

    val victory: Boolean = hasPlayerWon(1)
    val loss   : Boolean = hasPlayerWon(2)

    if (victory) 1 else if (loss) -1 else 0
  }

  private def isMoveLeft = data.exists(_.exists(_ == 0))

  private def hasPlayerWon(player: Int) = {
    val col0    = data(0)
    val col1    = data(1)
    val col3    = data(2)
    val victory = (col0(0) == player &&
                   col0(1) == player &&
                   col0(2) == player) ||
                  (col1(0) == player &&
                   col1(1) == player &&
                   col1(2) == player) ||
                  (col3(0) == player &&
                   col3(1) == player &&
                   col3(2) == player) ||
                  (col0(0) == player &&
                   col1(0) == player &&
                   col3(0) == player) ||
                  (col0(1) == player &&
                   col1(1) == player &&
                   col3(1) == player) ||
                  (col0(2) == player &&
                   col1(2) == player &&
                   col3(2) == player) ||
                  (col0(0) == player &&
                   col1(1) == player &&
                   col3(2) == player) ||
                  (col3(0) == player &&
                   col1(1) == player &&
                   col0(2) == player)
    victory
  }
  private val data              = Array.fill[Int](3, 3)(0)
  private var currentTurnPlayer = 1
  private val boardSizeRange    = 0 until 3

  override def undo(move: PlaceMarker): Unit = {
    data(move.x)(move.y) = 0
    flipPlayer()
  }

  override def validMoves: Iterator[PlaceMarker] = {
    val allMoves = boardSizeRange flatMap { x =>
      boardSizeRange flatMap { y =>
        if (data(x)(y) == 0) {
          Some(PlaceMarker(x, y, currentTurnPlayer))
        } else {
          None
        }
      }
    }
    allMoves.iterator
  }

  def flipPlayer(): Unit = {
    currentTurnPlayer = otherPlayer
  }

  override def applied(move: PlaceMarker): Unit = {
    data(move.x)(move.y) = move.player
    flipPlayer()
  }
  override def boardState: BoardState = {
    if (hasPlayerWon(1) || hasPlayerWon(2))
      WinnerDetermined
    else if (isMoveLeft) {
      OutcomeNotDetermined
    } else {
      MovesExhausted
    }
  }
}

object TicTacToeRating extends BoardRating[PlaceMarker, TicTacToeBoard] {
  override def rateLeaf(situation: TicTacToeBoard): Int = {
    situation.rateSelfAsSeenFromPlayerOne()
  }
}

object TicTacToePrinter extends BoardPrinter[PlaceMarker, TicTacToeBoard] {
  override def printBoard(board: TicTacToeBoard): String = {
    (0 to 2)
      .map { y =>
        (0 to 2).map { x =>
          board.playerOn(x, y) match {
            case 0 => " "
            case 1 => "X"
            case 2 => "O"
          }
        }.mkString
      }
      .mkString("\n")
  }
}

object TicTacToeBoard {
  def main(args: Array[String]): Unit = {
    val board = new TicTacToeBoard
    val ctx   = new GameContext[PlaceMarker, TicTacToeBoard](
      board,
      10,
      None,
      TicTacToeRating,
      TicTacToePrinter,
      true
    )
    AutoPlay.playTwoPlayerGame(ctx)
  }
}
