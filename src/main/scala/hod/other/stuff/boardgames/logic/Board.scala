package hod.other.stuff.boardgames.logic

import java.text.{DecimalFormat, DecimalFormatSymbols}
import scala.collection.mutable
import scala.io.AnsiColor

trait Board[M <: Move] {
  def validMoves: Iterator[M]
  def applied(move: M)
  def boardState: BoardState
  def gameNotFinished: Boolean = boardState == OutcomeNotDetermined
  def isTurnOfMaximizingPlayer: Boolean
}

sealed trait BoardState
case object OutcomeNotDetermined extends BoardState
case object MovesExhausted extends BoardState
case object WinnerDetermined extends BoardState

trait MoveCacheSupport[M <: Move, B <: Board[M]] {
  def clear(): Unit
  def isCacheSupported(depth: Int): Boolean
  def store(b: B, rating: Int): Unit
  def hasRatingStored(b: B): Boolean
  def getRating(b: B): Int
}

object MoveCacheSupport {
  def noSupport[M <: Move, B <: Board[M]]: MoveCacheSupport[M, B] = new MoveCacheSupport[M, B] {
    override def clear(): Unit = {}
    override def isCacheSupported(depth: Int): Boolean = false
    override def store(b: B, rating: Int): Unit =
      throw new UnsupportedOperationException()
    override def hasRatingStored(b: B): Boolean =
      throw new UnsupportedOperationException()
    override def getRating(b: B): Int =
      throw new UnsupportedOperationException()
  }
}

trait ImmutableBoard[M <: Move] extends Board[M]
trait MutableBoard[M <: Move] extends Board[M] {

  def undo(move: M): Unit
  def applyToMe(move: M): Unit = {
    applied(move)
  }
}

trait Move

trait BoardRating[M <: Move, B <: Board[M]] {
  def supportsNodeRatingAtDepth(i: Int): Boolean = false
  def rateNode(situation: B): Int = 0
  def rateLeaf(situation: B): Int
}

trait BoardPrinter[M <: Move, B <: Board[M]] {

  def colored(str: String, color: String) = s"$color$str${AnsiColor.RESET}"
  def printBoard(board: B): String
  def printMove(move: M, board: B): String = move.toString
}

class GameContext[M <: Move, B <: MutableBoard[M]](
                                                    val board: B,
                                                    val maxSearchDepth: Int,
                                                    val maxLeafEvals: Option[Int],
                                                    val rating: BoardRating[M, B],
                                                    val printer: BoardPrinter[M, B],
                                                    val alphaBetaPruning: Boolean = true,
                                                    val abortOnLoop: Boolean = true,
                                                    val moveCacheSupport: Option[MoveCacheSupport[M, B]] = None
                                                  ) {
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
    val start = System.currentTimeMillis()
    case class SearchResult(move: M, nodes: Int, leafs: Int, rating: Int, cached: Int, cacheHits: Int)
    def evalWithMaxDepth(depthLimit: Int): SearchResult = {
      val cacheSupport =
        context.moveCacheSupport.getOrElse(MoveCacheSupport.noSupport)
      cacheSupport.clear()
      var nodes     = 0
      var leafs     = 0
      var cached    = 0
      var cacheHits = 0

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

        def rateRecursive = {
          situation.applyToMe(move) // after this, it's the other player's turn
          def ratingOfCurrent = {
            leafs += 1
            rating.rateLeaf(situation)
          }

          val moveRating = remainingDepth match {
            case 0 =>
              ratingOfCurrent
            case _ =>
              situation.boardState match {
                case OutcomeNotDetermined =>
                  var myAlpha = alpha
                  var myBeta  = beta

                  val ratings         = {
                    val sortedMoves = {
                      if (context.rating.supportsNodeRatingAtDepth(depthLimit - remainingDepth)) {
                        situation.validMoves.toList.sortBy { nextMoveOption =>
                          situation.applyToMe(nextMoveOption)
                          val quickRating = {
                            if (maximizingPlayersTurn)
                              context.rating.rateNode(situation)
                            else
                              -context.rating.rateNode(situation)
                          }

                          situation.undo(nextMoveOption)
                          quickRating
                        }.iterator
                      } else {
                        situation.validMoves
                      }
                    }
                    sortedMoves.map { move =>
                      valueOfMove(
                        move,
                        remainingDepth - 1,
                        !maximizingPlayersTurn,
                        myAlpha,
                        myBeta
                      )
                    }
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

        if (cacheSupport.isCacheSupported(depthLimit - remainingDepth)) {
          if (cacheSupport.hasRatingStored(situation)) {
            cacheHits += 1
            cacheSupport.getRating(situation)
          } else {
            cached += 1
            val ratingOfThisMove = rateRecursive
            situation.applyToMe(move)
            cacheSupport.store(situation, ratingOfThisMove)
            situation.undo(move)
            ratingOfThisMove
          }
        } else {
          rateRecursive
        }

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

      val moves = context.board.validMoves.map { move =>
        move -> rateMove(move)
      }.toList

      val bestRating = {
        val justRatings = moves.iterator.map(_._2)
        if (isMaximizingPlayer) {
          justRatings.max
        } else {
          justRatings.min
        }
      }

      SearchResult(moves.find(_._2 == bestRating).get._1, nodes, leafs, bestRating, cached, cacheHits)
    }

    var maxDepth                                = 0
    val result = {
      context.maxLeafEvals match {
        case Some(leafs) =>
          var fallback   = Option.empty[SearchResult]
          val depthRange = 1 to context.maxSearchDepth
          depthRange.iterator
                    .map { tryDepth =>
                      val trip = evalWithMaxDepth(tryDepth)
                      fallback = Some(trip)
                      maxDepth = tryDepth
                      trip
                    }
                    .find(_.leafs > leafs)
                    .orElse(fallback)
                    .get
        case None =>
          maxDepth = context.maxSearchDepth
          evalWithMaxDepth(context.maxSearchDepth)
      }
    }

    if (debug) {
      val end         = System.currentTimeMillis()
      val elapsed     = (end - start) / 1000.0
      val bestMoveLog = context.printer.printMove(result.move, context.board)
      val sym         = new DecimalFormatSymbols()
      sym.setGroupingSeparator('.')
      val df        = new DecimalFormat("###,###,###,###", sym)
      val dfSeconds = new DecimalFormat("#0.00")
      println {
        s"Best move is $bestMoveLog " +
        s"with a rating of ${df.format(result.rating)} " +
        s"after ${df.format(result.nodes)} nodes/ ${df.format(result.leafs)} leafs " +
        s"at depth $maxDepth, " +
        s"${df.format(result.cacheHits)} cache hits, " +
        s"${df.format(result.cached)} cache size " +
        s"in ${dfSeconds.format(elapsed)}s"
      }
    }
    result.move
  }

}
