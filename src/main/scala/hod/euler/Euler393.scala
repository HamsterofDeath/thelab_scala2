package hod.euler

import java.text.DecimalFormat
import scala.collection.{BitSet, mutable}
import scala.io.AnsiColor

object Euler393 {
  def main(args: Array[String]): Unit = {
    def solveFor(size: Int) = {
      sealed trait Move {
        val arrow:Char
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

      val possibleMoves = {
        val moves = Array.tabulate[List[Move]](size, size)((x, y) => {
          var movesAtPosition = List.empty[Move]
          if (x > 0) {
            movesAtPosition :+= Left
          }
          if (x < size - 1) {
            movesAtPosition :+= Right
          }
          if (y > 0) {
            movesAtPosition :+= Up
          }
          if (y < size - 1) {
            movesAtPosition :+= Down
          }
          movesAtPosition
        })
        moves
      }

      def extractStateAtPosition(encoded: Long, x: Int) = {
        (encoded >> (x * 3)) & 7 match {
          case Left.bitCode => Left
          case Right.bitCode => Right
          case Up.bitCode => Up
          case Down.bitCode => Down
          case Undefined.bitCode => Undefined
        }
      }

      class EncodedState(size:Int) {
        private val data = Array.fill[Long](size)(Undefined.bitCode)

        def bitsAt(x: Int, y: Int) = {
          (data(y) >> (x * 3)) & 7
        }

        def getEncodedRow(y: Int) = data(y)

        def moveAt(x: Int, y: Int) = extractStateAtPosition(data(y), x)

        def setMove(x:Int,y:Int, move:Move)  ={
          data(y) |= move.bitCode << x*3
        }
        def resetMove(x:Int,y:Int)  ={
          data(y) &= ~(7 << x*3)
        }

        override def toString: String = {
          (0 until size).map { y=>
            (0 until size). map { x =>
              moveAt(x, y).arrow
            }.mkString
          }.mkString("\n")
        }
      }
      val moveHistory = new EncodedState(size)

      var cacheRequests = 0L
      var cacheMisses = 0L
      def cacheHits = cacheRequests-cacheMisses
      val subResultCache = mutable.HashMap.empty[StateKey, Long]


      case class StateKey(encodedRow1:Long, encodedRow2: Long, row: Int) {
        private def print(encoded:Long) = {
          (0 until size).map { x =>
            extractStateAtPosition(encoded, x)
          }.map(_.arrow).mkString

        }
        override def toString = {
          s"${print(encodedRow1)}\n${print(encodedRow2)}"
        }
      }
      def isInField(x: Int, y: Int) = {
        x >= 0 && x < size && y >= 0 && y < size
      }

      def nextRow(y: Int): Long = {

        def nextCell(x: Int): Long = {
          if (x == size && y < size - 1) {
            nextRow(y + 1)
          } else if (x == size) {
            1
          } else {
            val traverseMoves = possibleMoves(x)(y)
            def isTarget(x:Int, y:Int) = {
              !isInField(x, y) ||
              ensure(x-1,y, Right) ||
              ensure(x+1,y, Left) ||
              ensure(x,y-1, Down) ||
              ensure(x,y+1, Up)
            }

            def ensureNot(x: Int, y: Int, move: Move) = {
              if (isInField(x, y))
                moveHistory.moveAt(x,y) != move
              else true
            }

            def ensure(x: Int, y: Int, move: Move) = {
              if (isInField(x, y))
                moveHistory.moveAt(x,y) == move
              else false
            }
            def makeMove(move: Move): Long = {
              moveHistory.setMove(x,y,move)
              val count = nextCell(x + 1)
              moveHistory.resetMove(x, y)
              count
            }
            traverseMoves.map {
              case Left
                  if ensureNot(x - 1, y, Right) &&
                    ensureNot(x - 2, y, Right) &&
                    ensureNot(x - 1, y - 1, Down) &&
                    isTarget(x,y-1) =>
                makeMove(Left)
              case Right if
              ensureNot(x + 1, y - 1, Down) &&
              isTarget(x,y-1)
              =>
                makeMove(Right)
              case Up
                  if ensureNot(x, y - 1, Down) &&
                    ensureNot(x, y - 2, Down) &&
                    ensureNot(x - 1, y - 1, Right) &&
                    ensureNot(x + 1, y - 1, Left) &&
                    !isTarget(x,y-1)=>
                makeMove(Up)
              case Down if isTarget(x,y-1) =>
                makeMove(Down)
              case _ =>
                0
            }.sum
          }
        }

        def evalFromHere = {
          nextCell(0)
        }

        if (y > 1) {
          val key = {
            StateKey(
              moveHistory.getEncodedRow(y - 2),
              moveHistory.getEncodedRow(y-1),
              y
            )
          }
          cacheRequests += 1
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
      println(s"Cache: ${cacheHits.nice}/${cacheRequests.nice}, size ${subResultCache.size.nice}")
      result
    }

    val solution = solveFor(8)
    println(solution)

  }
}
