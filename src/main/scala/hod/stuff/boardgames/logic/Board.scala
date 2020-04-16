package hod.stuff.boardgames.logic

import scala.collection.mutable
import scala.util.Random

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

trait BoardRating[M <: Move, B <: Board[M]] {
  def rate(situation: B): Int
}

trait BoardPrinter[M <: Move, B <: Board[M]] {
  object Colors {
    val ANSI_RESET  = "\u001B[0m"
    val ANSI_BLACK  = "\u001B[30m"
    val ANSI_RED    = "\u001B[31m"
    val ANSI_GREEN  = "\u001B[32m"
    val ANSI_YELLOW = "\u001B[33m"
    val ANSI_BLUE   = "\u001B[34m"
    val ANSI_PURPLE = "\u001B[35m"
    val ANSI_CYAN   = "\u001B[36m"
    val ANSI_WHITE  = "\u001B[37m"
  }

  def colored(str: String, color: String) = s"$color$str${Colors.ANSI_RESET}"
  def printBoard(board: B): String
  def printMove(move: M, board: B): String = move.toString
}

class GameContext[M <: Move, B <: MutableBoard[M]](
                                                    val board: B,
                                                    val maxSearchDepth: Int,
                                                    val maxLeafEvals: Option[Int],
                                                    val rating: BoardRating[M, B],
                                                    val printer: BoardPrinter[M, B],
                                                    val stableMoveChoice: Boolean = true,
                                                    val alphaBetaPruning: Boolean = true,
                                                    val abortOnLoop: Boolean = true
                                                  ) {
  if (alphaBetaPruning) require(stableMoveChoice)
  def printForConsole = {
    printer.printBoard(board)
  }
}

class GameLockedInDraw extends Exception
class GameEndsWithWinner extends Exception

trait BoardPlayer[M <: Move, B <: MutableBoard[M]] {
  def makeMove(context: GameContext[M, B]): M
}

object BoardPlayer {
  def autoPlayer[M <: Move, B <: MutableBoard[M]] = new AutoPlayer[M, B]
}
class AutoPlayer[M <: Move, B <: MutableBoard[M]] extends BoardPlayer[M, B] {
  override def makeMove(context: GameContext[M, B]): M =
    MoveTraverse.searchBestMove(context)
}

object AutoPlay {
  def playTwoPlayerGame[M <: Move, B <: MutableBoard[M]](
                                                          context: GameContext[M, B],
                                                          firstPlayer: BoardPlayer[M, B] = BoardPlayer.autoPlayer[M, B],
                                                          secondPlayer: BoardPlayer[M, B] = BoardPlayer.autoPlayer[M, B]
                                                        ): Unit = {
    val moveHistory = mutable.ArrayBuffer.empty[M]
    println(s"Board:\n${context.printForConsole}")

    var looping = false

    while (!looping && context.board.gameNotFinished) {

      val moveOfPlayer = {
        val isFirstPlayerTurn = context.board.isTurnOfMaximizingPlayer
        if (isFirstPlayerTurn) {
          firstPlayer.makeMove(context)
        } else {
          secondPlayer.makeMove(context)
        }
      }

      moveHistory += moveOfPlayer
      println(
        s" -> Move: ${context.printer.printMove(moveOfPlayer, context.board)}"
      )

      context.board.applyToMe(moveOfPlayer)

      println(s"Board:\n${context.printForConsole}\n")

      if (context.abortOnLoop && moveHistory.size > 10) {
        2.to(10, 2).foreach { moves =>
          val last =
            moveHistory.slice(moveHistory.size - moves, moveHistory.size)
          val prev = moveHistory
            .slice(moveHistory.size - moves - moves, moveHistory.size - moves)
          if (last == prev) {
            looping = true
          }
        }
      }
    }

    println("game over")

    if (looping) {
      println("moves were looping")
    }
  }

}

object MoveTraverse {
  private val debug = true
  private case class MoveAndRating[M <: Move](move: M, rating: Int)

  def searchBestMove[M <: Move, B <: MutableBoard[M]](
                                                       context: GameContext[M, B]
                                                     ): M = {

    def evalWithMaxDepth(depthLimit: Int) = {
      var nodes = 0
      var leafs = 0

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

      val rating    = context.rating
      val situation = context.board

      def valueOfMove(move: M,
                      remainingDepth: Int,
                      maximizingPlayersTurn: Boolean,
                      alpha: Int,
                      beta: Int): Int = {
        nodes += 1

        situation.applyToMe(move) // after this, it's the other player's turn
        def ratingOfCurrent = {
          leafs += 1
          rating.rate(situation)
        }

        val moveRating = remainingDepth match {
          case 0 =>
            ratingOfCurrent
          case _ =>
            situation.boardState match {
              case OutcomeNotDetermined =>
                var myAlpha = alpha
                var myBeta  = beta

                val ratings         = situation.validMoves.map { move =>
                  valueOfMove(
                    move,
                    remainingDepth - 1,
                    !maximizingPlayersTurn,
                    myAlpha,
                    myBeta
                  )
                }
                // this is flipped because the move to be tested has already been made
                val valueOfBestMove = {
                  if (maximizingPlayersTurn) {
                    val pruned = takeUntil(ratings) { rating =>
                      if (context.alphaBetaPruning) {
                        myBeta = myBeta min rating
                        val take = alpha < myBeta
                        take
                      } else {
                        true
                      }
                    }
                    pruned.min
                  } else {
                    val pruned = takeUntil(ratings) { rating =>
                      if (context.alphaBetaPruning) {
                        myAlpha = myAlpha max rating
                        val take = myAlpha < beta
                        take
                      } else {
                        true
                      }
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

      var rootAlpha          = Int.MinValue
      var rootBeta           = Int.MaxValue
      val isMaximizingPlayer = situation.isTurnOfMaximizingPlayer

      def rateMove(move: M) = {
        val rating =
          valueOfMove(move, depthLimit, isMaximizingPlayer, rootAlpha, rootBeta)
        if (isMaximizingPlayer) {
          rootAlpha = rootAlpha max rating
        } else {
          rootBeta = rootBeta min rating
        }

        rating
      }

      val moves = context.board.validMoves.toList.map { move =>
        move -> rateMove(move)
      }

      val bestRating = {
        val justRatings = moves.iterator.map(_._2)
        if (isMaximizingPlayer) {
          justRatings.max
        } else {
          justRatings.min
        }
      }

      val randomMove = {
        val candidates = moves.filter(_._2 == bestRating).map(_._1)
        if (context.stableMoveChoice) {
          candidates.head
        } else {
          candidates(new Random().nextInt(candidates.size))
        }
      }
      (randomMove, leafs, bestRating)
    }

    var maxDepth = 0
    val bestMove = context.maxLeafEvals match {
      case Some(leafs) =>
        var fallback = Option.empty[(M, Int, Int)]
        (1 to context.maxSearchDepth).iterator
                                     .map { tryDepth =>
                                       val trip = evalWithMaxDepth(tryDepth)
                                       fallback = Some(trip)
                                       maxDepth = tryDepth
                                       trip
                                     }
                                     .find(_._2 > leafs)
                                     .orElse(fallback)
                                     .get
      case None =>
        maxDepth = context.maxSearchDepth
        evalWithMaxDepth(context.maxSearchDepth)
    }

    if (debug) {
      println(s"Best move is ${context.printer.printMove(bestMove._1, context.board)} with a rating of ${bestMove._3} after ${
        bestMove._2
      } checks at depth $maxDepth")
    }
    bestMove._1
  }

}
