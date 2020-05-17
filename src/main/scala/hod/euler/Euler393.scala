package hod.euler

import java.text.DecimalFormat
import scala.collection.{BitSet, mutable}
import scala.io.AnsiColor
import scala.util.Random

object Euler393 {
  sealed trait Move {
    val arrow: Char
    val bitCode: Int
    val shiftX: Int
    val shiftY: Int
  }

  case object Undefined extends Move {
    override val shiftX: Int = 0
    override val shiftY: Int = 0
    override val bitCode: Int = 0
    override val arrow: Char = ' '
  }
  case object Left extends Move {
    override val shiftX: Int = -1
    override val shiftY: Int = 0
    override val bitCode: Int = 1
    override val arrow: Char = '←'
  }
  case object Right extends Move {
    override val shiftX: Int = 1
    override val shiftY: Int = 0
    override val bitCode: Int = 2
    override val arrow: Char = '→'
  }

  case object Up extends Move {
    override val shiftX: Int = 0
    override val shiftY: Int = -1
    override val bitCode: Int = 3
    override val arrow: Char = '↑'
  }

  case object Down extends Move {
    override val shiftX: Int = 0
    override val shiftY: Int = 1
    override val bitCode: Int = 4
    override val arrow: Char = '↓'
  }

  def main(args: Array[String]): Unit = {
    def solveFor(size: Int) = {

      val possibleMoves = {
        val moves = Array.tabulate[List[Move]](size, size)((x, y) => {
          var movesAtPosition = List.empty[Move]
          if (y > 0) {
            movesAtPosition :+= Up
          }
          if (y < size - 1) {
            movesAtPosition :+= Down
          }
          if (x > 0) {
            movesAtPosition :+= Left
          }
          if (x < size - 1) {
            movesAtPosition :+= Right
          }
          movesAtPosition
        })
        moves
      }

      def extractStateAtPosition(encoded: Long, x: Int) = {
        (encoded >> (x * 3)) & 7 match {
          case Left.bitCode      => Left
          case Right.bitCode     => Right
          case Up.bitCode        => Up
          case Down.bitCode      => Down
          case Undefined.bitCode => Undefined
        }
      }

      class EncodedState(size: Int) {
        private val data = Array.fill[Long](size)(Undefined.bitCode)
        private val targeted = Array.fill[Boolean](size, size)(false)

        def ensureNot(x: Int, y: Int, move: Move) = {
          if (isInside(x, y))
            bitsAt(x, y) != move.bitCode
          else true
        }

        def ensure(x: Int, y: Int, move: Move) = {
          if (isInside(x, y))
            bitsAt(x, y) == move.shiftX
          else false
        }

        def isInside(x: Int, y: Int) = {
          x >= 0 && x < size && y >= 0 && y < size
        }

        def isTarget(x: Int, y: Int) = {
          !isInside(x, y) || targeted(x)(y)
        }

        private def countInRow(y: Int, move: Move) = {
          val bits = move.bitCode
          (0 until size).count { x =>
            bitsAt(x, y) == bits
          }
        }

        def bitsAt(x: Int, y: Int) = {
          (data(y) >> (x * 3)) & 7
        }

        def getEncodedRow(y: Int) = data(y)

        def moveAt(x: Int, y: Int) = extractStateAtPosition(data(y), x)

        def setMove(x: Int, y: Int, move: Move) = {
          data(y) |= move.bitCode << x * 3
          targeted(x + move.shiftX)(y + move.shiftY) = true

        }
        def resetMove(x: Int, y: Int, move:Move) = {
          data(y) &= ~(7 << x * 3)
          targeted(x + move.shiftX)(y + move.shiftY) = false
        }

        override def toString: String = {
          (0 until size)
            .map { y =>
              (0 until size).map { x =>
                moveAt(x, y).arrow
              }.mkString
            }
            .mkString("\n")
        }
      }
      val moveHistory = new EncodedState(size)

      var cacheRequests = 0L
      var cacheMisses = 0L
      var moves = 0L
      def cacheHits = cacheRequests - cacheMisses
      val subResultCache = mutable.HashMap.empty[StateKey, Long]

      case class StateKey(encodedRows: Long, row: Int) {
        private def print(encoded: Long) = {
          (0 until size)
            .map { x =>
              extractStateAtPosition(encoded, x)
            }
            .map(_.arrow)
            .mkString

        }

        def encodedRow1 = encodedRows>>32
        def encodedRow2 = encodedRows << 32 >> 32
        override def toString = {
          s"${print(encodedRow1)}\n${print(encodedRow2)}"
        }
      }
      def isInField(x: Int, y: Int) = {
        moveHistory.isInside(x, y)
      }

      def nextRow(y: Int): Long = {

        def nextCell(x: Int): Long = {
          if (x == size && y < size - 1) {
            nextRow(y + 1)
          } else if (x == size) {
            1
          } else {
            val traverseMoves = possibleMoves(x)(y)

            def makeMove(move: Move): Long = {
              moveHistory.setMove(x, y, move)

              val count = {
                def evaluateNextStep = {
                  moves += 1
                  nextCell(x + 1)
                }
                evaluateNextStep
              }
              moveHistory.resetMove(x, y, move)
              count
            }

            def canGoLeft = {
              moveHistory.ensureNot(x - 1, y, Right) &&
              moveHistory.ensureNot(x - 2, y, Right) &&
              moveHistory.ensureNot(x - 1, y - 1, Down) &&
              moveHistory.isTarget(x, y - 1)
            }

            def canGoRight = {
              moveHistory.ensureNot(x + 1, y - 1, Down) &&
              moveHistory.isTarget(x, y - 1) &&
              (y < size - 1 || moveHistory.isTarget(x - 1, y)) &&
              (moveHistory.ensureNot(x - 1, y, Down) || moveHistory.isTarget(x - 1, y))
            }

            def canGoUp = {
              moveHistory.ensureNot(x, y - 1, Down) &&
              moveHistory.ensureNot(x, y - 2, Down) &&
              moveHistory.ensureNot(x - 1, y - 1, Right) &&
              moveHistory.ensureNot(x + 1, y - 1, Left) &&
              (y + 2 != size || !moveHistory.ensure(x - 1, y, Down) || moveHistory.isTarget(x - 1, y)) &&
              !moveHistory.isTarget(x, y - 1)
            }

            def canGoDown = {
              moveHistory.isTarget(x, y - 1) &&
              (x < size - 1 || moveHistory.isTarget(x, y)) &&
              (moveHistory.isTarget(x - 1, y) || moveHistory.ensureNot(x - 1, y, Down))
            }

            val subSum = traverseMoves // Random .shuffle(traverseMoves)
            .map {
              case Left if canGoLeft   => makeMove(Left)
              case Down if canGoDown   => makeMove(Down)
              case Right if canGoRight => makeMove(Right)
              case Up if canGoUp       => makeMove(Up)
              case _                   => 0 // dead end
            }.sum
            if (subSum == 0) {
              noop()
            }
            subSum
          }
        }

        def evalFromHere = {
          nextCell(0)
        }

        if (y > 1) {
          val key = {
            StateKey(
              moveHistory.getEncodedRow(y - 2)<<32 | moveHistory.getEncodedRow(y - 1),
              y
            )
          }
          cacheRequests += 1
          if (cacheRequests % 1000000 == 0) {
            println(
              s"Cache: ${cacheHits.nice}/${cacheRequests.nice}, size ${subResultCache.size.nice}"
            )

          }
          val fromCache = subResultCache.getOrElseUpdate(key, {
            cacheMisses += 1
            evalFromHere
          })
          fromCache
        } else {
          evalFromHere
        }
      }

      val result = nextRow(0)
      println(
        s"Cache: ${cacheHits.nice}/${cacheRequests.nice}, size ${subResultCache.size.nice}"
      )
      println(s"Moves: ${moves.nice}")
      result
    }

    //require (207408==solveFor(6))
    val solution = solveFor(8)
    println(solution.nice)

  }
}
