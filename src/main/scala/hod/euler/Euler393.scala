package hod.euler

import scala.io.AnsiColor

import hod.stuff.boardgames.logic.Move

object Euler393 {
  def main(args: Array[String]): Unit = {

    def countValidSetups(nx: Int, ny: Int) = {
      case class Position(x: Int, y: Int)
      case class Move(from: Position, to: Position)

      def toString(moves: List[Move]) = {
        val picture = Array.fill[String](nx, ny)(" ")
        var previousMove = moves.head
        val colors = Iterator
          .continually(
            List(
              AnsiColor.BLUE,
              AnsiColor.GREEN,
              AnsiColor.RED,
              AnsiColor.YELLOW
            )
          )
          .flatten
        var colorToUse = colors.next()
        moves.reverse.foreach { move =>
          if (previousMove != move && previousMove.to != move.from) {
            colorToUse = colors.next()
          }
          val xDiff = move.to.x - move.from.x
          val yDiff = move.to.y - move.from.y
          if (xDiff.abs + yDiff.abs == 1) {
            val insert = (xDiff, yDiff) match {
              case (-1, 0) => '←'
              case (0, 1)  => '↑'
              case (1, 0)  => '→'
              case (0, -1) => '↓'
              case _       => throw new RuntimeException
            }
            val insertMe = insert
            val sourceChar = insertMe
            picture(move.from.y)(move.from.x) = {
              s"${colorToUse}${sourceChar}${AnsiColor.RESET}"
            }
          }
          previousMove = move
        }

        (picture.map(_.mkString).reverse.mkString("\n") + "\n-\n")
      }

      val positions = Array.tabulate(nx, ny)((nx, ny) => Position(nx, ny))

      def posAt(x: Int, y: Int) = positions(x)(y)
      def adjacent(pos: Position) = {
        var valid = List.empty[Position]
        if (pos.x > 0) {
          valid = posAt(pos.x - 1, pos.y) :: valid
        }
        if (pos.x < nx - 1) {
          valid = posAt(pos.x + 1, pos.y) :: valid
        }
        if (pos.y > 0) {
          valid = posAt(pos.x, pos.y - 1) :: valid
        }
        if (pos.y < ny - 1) {
          valid = posAt(pos.x, pos.y + 1) :: valid
        }
        valid
      }

      val taken = Array.fill[Boolean](nx, ny)(false)

      def anyUnprocessedPosition: Position = {
        0 until nx foreach { x =>
          0 until ny foreach { y =>
            if (!taken(x)(y))
              return posAt(x, y)
          }
        }
        throw new IllegalStateException()
      }

      var antChainStart = posAt(0, 0)

      def countSubMoves(start: Position,
                        blocked: Position,
                        antsLeft: Int,
                        debug: List[Move]): Int = {
        if (antsLeft == 0) {
          println(toString(debug))
          1
        } else {
          val moves = adjacent(start)
            .filter { p =>
              p != blocked && !taken(p.x)(p.y)
            }

          moves.map { nextPosition =>
            taken(nextPosition.x)(nextPosition.y) = true
            val isChainComplete = nextPosition == antChainStart
            val antsLeftAfterThis = antsLeft - 1
            val validSetups =
              if (isChainComplete) {
                if (antsLeftAfterThis > 0) {
                  val originalChainStart = antChainStart
                  antChainStart = anyUnprocessedPosition
                  val subSum = countSubMoves(
                    antChainStart,
                    antChainStart,
                    antsLeftAfterThis,
                    Move(start, originalChainStart) :: debug
                  )
                  antChainStart = originalChainStart
                  subSum
                } else {
                  countSubMoves(
                    antChainStart,
                    antChainStart,
                    antsLeftAfterThis,
                    Move(start, antChainStart) :: debug
                  )
                }
              } else {
                countSubMoves(
                  nextPosition,
                  start,
                  antsLeftAfterThis,
                  Move(start, nextPosition) :: debug
                )
              }

            taken(nextPosition.x)(nextPosition.y) = false
            validSetups
          }.sum
        }
      }
      countSubMoves(antChainStart, antChainStart, nx * ny, Nil)
    }

    val solution = measured {
      countValidSetups(6, 6)
    }
    println(solution)
  }
}
