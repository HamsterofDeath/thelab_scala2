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

trait BoardPrinter[M <: Move, B <: Board[M]] {
  def printBoard(board: B): String
  def printMove(move: M, board: B): String = move.toString
}

class GameContext[M <: Move, B <: MutableBoard[M]](val board: B, val searchDepth: Int, val rating: Rating[M, B], val printer: BoardPrinter[M, B]) {
  def printForConsole = {
    printer.printBoard(board)
  }
}

class GameLockedInDraw extends Exception
class GameEndsWithWinner extends Exception

object AutoPlay {
  def playTwoPlayerGame[M <: Move, B <: MutableBoard[M]](context: GameContext[M, B]): Unit = {
    println(s"Board:\n${context.printForConsole}")
    while (context.board.gameNotFinished) {
      val best = MoveTraverse.searchBestMove(context)
      println(s" -> Move: ${context.printer.printMove(best, context.board)}")
      context.board.applyToMe(best)
      println(s"Board:\n${context.printForConsole}\n")
    }
    println("game over")
  }

}

object MoveTraverse {
  private val debug = false
  private case class MoveAndRating[M <: Move](move: M, rating: Int)

  def searchBestMove[M <: Move, B <: MutableBoard[M]](context: GameContext[M, B]): M = {
    def takeUntil[T](it: IterableOnce[T])(continueIf: T => Boolean) = {
      var oneMoreAdded = false
      it.iterator.takeWhile { e =>
        val defaultCheck = continueIf(e)
        val addOne       = !oneMoreAdded
        if (!defaultCheck) {
          oneMoreAdded = true
        }

        defaultCheck || addOne
      }
    }

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

              val ratings         = situation.validMoves.map { move =>
                valueOfMove(move, remainingDepth - 1, !maximizingPlayersTurn, myAlpha, myBeta)
              }
              // this is flipped because the move to be tested has already been made
              val valueOfBestMove = {
                if (maximizingPlayersTurn) {
                  val pruned = takeUntil(ratings) { rating =>
                    myBeta = myBeta min rating
                    val take = alpha < myBeta
                    take
                  }
                  pruned.min
                } else {
                  val pruned = takeUntil(ratings) { rating =>
                    myAlpha = myAlpha max rating
                    val take = myAlpha < beta
                    take
                  }
                  pruned.max
                }
              }
              valueOfBestMove
            case WinnerDetermined | MovesExhausted =>
              // as seen from the other player
              ratingOfCurrent
          }

      }
      situation.undo(move)
      moveRating
    }

    val moves = context.board.validMoves.toList

    var rootAlpha          = Int.MinValue
    var rootBeta           = Int.MaxValue
    val isMaximizingPlayer = situation.isTurnOfMaximizingPlayer

    def rateMove(move: M) = {
      val rating = valueOfMove(move, context.searchDepth, isMaximizingPlayer, rootAlpha, rootBeta)
      if (isMaximizingPlayer) {
        rootAlpha = rootAlpha max rating
      } else {
        rootBeta = rootBeta min rating
      }

      if (debug) println(s"Candidate $move = $rating, player one: $isMaximizingPlayer, alpha: $rootAlpha, beta: $rootBeta")
      rating
    }

    if (isMaximizingPlayer) {
      moves.maxBy(rateMove)
    } else {
      moves.minBy(rateMove)
    }
  }

}
