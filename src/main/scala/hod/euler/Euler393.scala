package hod.euler

import scala.collection.{BitSet, mutable}
import scala.io.AnsiColor

object Euler393 {
  def main(args: Array[String]): Unit = {
    def solveFor(size: Int) = {
      sealed trait Move {
        def shiftX: Int
        def shiftY: Int
      }
      case object Left extends Move {
        override def shiftX: Int = -1
        override def shiftY: Int = 0
      }
      case object Right extends Move {
        override def shiftX: Int = 1
        override def shiftY: Int = 0
      }

      case object Up extends Move {
        override def shiftX: Int = 0
        override def shiftY: Int = -1
      }

      case object Down extends Move {
        override def shiftX: Int = 0
        override def shiftY: Int = 1
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

      val moveHistory = Array.fill[Move](size, size)(null: Move)
      val score = Array.fill[Int](size, size)(1)
      def debugIt = {
        (0 until size).map { y =>
          (0 until size).map { x =>
            moveHistory(x)(y) match {
              case Left => "←"
              case Right =>"→"
              case Up =>"↑"
              case Down =>"↓"
            }
          }.mkString
        }.mkString("\n")
      }
      def nextRow(y: Int): Long = {

        def nextCell(x: Int):Long = {
          if (x == size && y < size - 1) {
            nextRow(y + 1)
          } else if (x == size) {
            1
          } else {
            require(x >= 0)
            require(y >= 0)
            require(x < size)
            require(y < size)
            val traverseMoves = possibleMoves(x)(y)
            def ensureNot(x: Int, y: Int, move: Move) = {
              if (x >= 0 && x < size && y >= 0 && y < size)
                moveHistory(x)(y) != move
              else true
            }
            def makeMove(move:Move):Long = {
              moveHistory(x)(y) = move
              println(s"filled $x/$y with $move")
              val count = nextCell(x + 1)
              moveHistory(x)(y) = null
              println(s"cleared $x/$y")
              count
            }
            traverseMoves.map {
              case Left
                if ensureNot(x - 1, y, Right) &&
                   ensureNot(x - 1, y - 1, Down)         =>
                makeMove(Left)
              case Right if ensureNot(x + 1, y - 1, Down) =>
                makeMove(Right)
              case Up
                if ensureNot(x, y - 1, Down) &&
                   ensureNot(x - 1, y - 1, Right) &&
                   ensureNot(x + 1, y - 1, Left) =>
                makeMove(Up)
              case Down                                         =>
                makeMove(Down)
              case _ => 0
            }.sum
          }
        }
        nextCell(0)
      }
      nextRow(0)
    }

    val solution = solveFor(4)
    println(solution)
  }
}
