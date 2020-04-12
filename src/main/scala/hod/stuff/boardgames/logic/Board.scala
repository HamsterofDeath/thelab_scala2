package hod.stuff.boardgames.logic

trait Board[M <: Move] {
  def validMoves: Iterator[M]
  def applied(move: M)
  def boardState: BoardState
  def gameNotFinished = boardState == OutcomeNotDetermined
  def isTurnOfMaximizingPlayer: Boolean
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
    println(s"Board:\n${context.printForConsole}")
    while (context.board.gameNotFinished) {
      val best = MoveTraverse.searchBestMove(context)
      println(s"Move: $best")
      context.board.applyToMe(best)
      println(s"Board:\n${context.printForConsole}")
    }
  }
  println("game over")
}

object MoveTraverse {
  private val debug = true
  private case class MoveAndRating[M <: Move](move: M, rating: Int)

  def searchBestMove[M <: Move, B <: MutableBoard[M]](context: GameContext[M, B]): M = {
    val rating = context.rating

    val situation = context.board

    def valueOfMove(move: M, remainingDepth: Int, maximizingPlayersTurn: Boolean, alpha: Int, beta: Int): Int = {

      situation.applyToMe(move) // after this, it's the other player's turn
      def ratingOfCurrent = rating.rate(situation)

      val moveRating = remainingDepth match {
        case 0 =>
          ratingOfCurrent
        case _ =>
          situation.boardState match {
            case OutcomeNotDetermined =>
              var myAlpha = alpha
              var myBeta  = beta

              val ratings = situation.validMoves.toList.map { move =>
                valueOfMove(move, remainingDepth - 1, !maximizingPlayersTurn, myAlpha, myBeta)
              }
              // this is flipped because the move to be tested has already been made
              if (maximizingPlayersTurn) {
                ratings.takeWhile { rating =>
                  myBeta = myBeta min rating
                  alpha < myBeta
                  true
                }.min
              } else {
                ratings.takeWhile { rating =>
                  myAlpha = myAlpha max rating
                  myAlpha < beta
                  true
                }.max
              }
            case WinnerDetermined | MovesExhausted =>
              // as seen from the other player
              ratingOfCurrent
          }

      }
      situation.undo(move)
      moveRating
    }

    val moves = context.board.validMoves.toList

    def rateMove(move: M) = {
      val rating = valueOfMove(move, context.searchDepth, situation.isTurnOfMaximizingPlayer, Int.MinValue, Int.MaxValue)
      if (debug) println(s"Candidate $move = $rating, player one: ${situation.isTurnOfMaximizingPlayer}")
      rating
    }

    if (situation.isTurnOfMaximizingPlayer) {
      moves.maxBy(rateMove)
    } else {
      moves.minBy(rateMove)
    }
  }

}
