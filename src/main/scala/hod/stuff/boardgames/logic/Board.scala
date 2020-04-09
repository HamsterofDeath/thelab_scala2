package hod.stuff.boardgames.logic

trait Board[M <: Move] {
  def validMoves: Iterator[M]
  def applied(move: M)
  def boardState: BoardState
  def gameNotFinished = boardState == OutcomeNotDetermined
}

sealed trait BoardState
case object OutcomeNotDetermined extends BoardState
case object MovesExhausted extends BoardState
case object WinnerDetermined extends BoardState

trait ImmutableBoard[M <: Move] extends Board[M]
trait MutableBoard[M <: Move] extends Board[M] {

  def undo(move: M): Unit
  def applyToMe(move: M): Unit = {
    applied(move)
  }
}

trait Move

trait Rating[M <: Move, B <: Board[M]] {
  def rate(situation: B): Int
}

trait BoardPrinter[B <: Board[_]] {
  def toString(board: B): String
}

class GameContext[M <: Move, B <: MutableBoard[M]](val board: B, val searchDepth: Int, val rating: Rating[M, B], val printer: BoardPrinter[B]) {
  def printForConsole = {
    printer.toString(board)
  }
}

class GameLockedInDraw extends Exception
class GameEndsWithWinner extends Exception

object AutoPlay {
  def playTwoPlayerGame[M <: Move, B <: MutableBoard[M]](context: GameContext[M, B]): Unit = {
    while (context.board.gameNotFinished) {
      val best = MoveTraverse.searchBestMove(context)
      println(s"Move: $best")
      context.board.applyToMe(best)
      println(s"Board:\n${context.printForConsole}")
    }
  }
}

object MoveTraverse {
  private case class MoveAndRating[M <: Move](move: M, rating: Int) {
    def flipViewPoint = copy(rating = -rating)

  }

  def searchBestMove[M <: Move, B <: MutableBoard[M]](context: GameContext[M, B]): M = {
    val rating = context.rating

    val situation = context.board

    def valueOfMove(move: M, maxDepth: Int, myTurn: Boolean): Int = {
      situation.applyToMe(move)
      val moveRating         = maxDepth match {
        case 0 =>
          // as seen from the other player
          -rating.rate(situation)
        case _ =>
          situation.boardState match {
            case OutcomeNotDetermined =>
              val ratings = situation.validMoves.map { move =>
                valueOfMove(move, maxDepth - 1, !myTurn)
              }
              if (myTurn) ratings.min else ratings.max
            case WinnerDetermined | MovesExhausted =>
              // as seen from the other player
              -rating.rate(situation)
          }

      }
      val ratingForPlayerOne = if (myTurn) moveRating else -moveRating
      situation.undo(move)
      ratingForPlayerOne
    }

    context.board.validMoves.maxBy { move =>
      valueOfMove(move, context.searchDepth, true)
    }
  }

}
