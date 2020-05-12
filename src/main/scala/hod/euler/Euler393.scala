package hod.euler

import scala.collection.mutable
import scala.io.AnsiColor

object Euler393 {
  case class Position(x: Int, y: Int)
  case class Move(from: Position, to: Position)
  case class Dimensions(x: Int, y: Int)
  case class Shape(dimensions: Dimensions, data: Set[Position])(
    val upLeft: Position
  ) {
    def anyPosition = data.head

    def shapeSize: Int = data.size

    def contains(x: Int, y: Int) = data.exists(e => e.x == x && e.y == y)

  }
  case class MapData(grid: Array[Array[Boolean]], dimensions: Dimensions)

  def toString(moves: List[Move]) = {
    val nx = moves.flatMap(e => List(e.from.x, e.to.x)).max + 1
    val ny = moves.flatMap(e => List(e.from.y, e.to.y)).max + 1
    val picture = Array.fill[String](nx, ny)(" ")
    var previousMove = moves.head
    val colors = Iterator
      .continually(
        List(AnsiColor.BLUE, AnsiColor.GREEN, AnsiColor.RED, AnsiColor.YELLOW)
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

  def main(args: Array[String]): Unit = {

    val pathCountCache = mutable.HashMap.empty[Shape, Long]
    var cacheHits = 0
    def countValidSetups(nx: Int, ny: Int): Long = {
      //var solutions = List.empty[List[Move]]

      val positions = Array.tabulate(nx, ny)((nx, ny) => Position(nx, ny))

      def posAt(x: Int, y: Int) = positions(x)(y)
      def adjacent(pos: Position, dimensions: Dimensions) = {
        var valid = List.empty[Position]
        if (pos.x > 0) {
          valid = posAt(pos.x - 1, pos.y) :: valid
        }
        if (pos.x < dimensions.x - 1) {
          valid = posAt(pos.x + 1, pos.y) :: valid
        }
        if (pos.y > 0) {
          valid = posAt(pos.x, pos.y - 1) :: valid
        }
        if (pos.y < dimensions.y - 1) {
          valid = posAt(pos.x, pos.y + 1) :: valid
        }
        valid
      }

      val taken = Array.fill[Boolean](nx, ny)(false)

      var antChainStart = posAt(0, 0)
      val cacheEnabled = true
      def countSubMoves(start: Position,
                        previous: Position,
                        antsLeft: Int,
                        canCacheOnThisLevel: Boolean,
                        map: MapData): Long = {
        def anyUnprocessedPosition: Position = {
          map.grid.indices foreach { x =>
            map.grid(x).indices foreach { y =>
              if (!map.grid(x)(y))
                return posAt(x, y)
            }
          }
          throw new IllegalStateException()
        }

        if (antsLeft == 0) {
          1L
        } else if (canCacheOnThisLevel && cacheEnabled) {
          val shape = {
            val area = mutable.Set.empty[Position]
            def floodFill(current: Position): Unit = {
              area += current
              adjacent(current, map.dimensions).foreach { ad =>
                if (!map.grid(ad.x)(ad.y) && !area.contains(ad)) {
                  floodFill(ad)
                }
              }
            }
            floodFill(start)
            val minX = area.iterator.map(_.x).min
            val minY = area.iterator.map(_.y).min
            val maxX = area.iterator.map(_.x).max
            val maxY = area.iterator.map(_.y).max

            val locations = area.iterator.map { loc =>
              val relativeX = loc.x - minX
              val relativeY = loc.y - minY
              positions(relativeX)(relativeY)
            }.toSet
            val size = Dimensions(maxX - minX + 1, maxY - minY + 1)
            Shape(size, locations)(positions(minX)(minY))
          }

          cacheHits += 1

          val intermediateSum =
            pathCountCache.getOrElseUpdate(shape, {
              cacheHits -= 1
              if (shape.dimensions.x > 1 && shape.dimensions.y > 1) {
                val miniMap = {
                  MapData(
                    Array.tabulate(shape.dimensions.x, shape.dimensions.y)(
                      (x, y) => !shape.contains(x, y)
                    ),
                    shape.dimensions
                  )
                }
                val startAt = shape.anyPosition
                val originalChainStart = antChainStart
                antChainStart = startAt
                val subSum = countSubMoves(
                  startAt,
                  startAt,
                  shape.shapeSize,
                  false,
                  miniMap
                )
                antChainStart = originalChainStart
                subSum
              } else {
                0
              }
            })

          shape.data.foreach { loc =>
            map.grid(loc.x + shape.upLeft.x)(loc.y + shape.upLeft.y) = true
          }

          val ret = {
            if (antsLeft - shape.shapeSize > 0) {
              if (intermediateSum > 0) {
                val nextStart = anyUnprocessedPosition
                intermediateSum * countSubMoves(
                  nextStart,
                  nextStart,
                  antsLeft - shape.shapeSize,
                  true,
                  map
                )
              } else {
                0
              }
            } else {
              intermediateSum
            }
          }
          shape.data.foreach { loc =>
            map.grid(loc.x + shape.upLeft.x)(loc.y + shape.upLeft.y) = false
          }
          ret
        } else {

          def evalOptions = {
            val moves = adjacent(start, map.dimensions)
              .filter { p =>
                p != previous && !map.grid(p.x)(p.y)
              }

            moves.map { nextPosition =>
              map.grid(nextPosition.x)(nextPosition.y) = true
              val isChainComplete = nextPosition == antChainStart
              val antsLeftAfterThis = antsLeft - 1
              val validSetups = {
                if (isChainComplete) {
                  if (antsLeftAfterThis > 0) {
                    val originalChainStart = antChainStart
                    antChainStart = anyUnprocessedPosition
                    val subSum = countSubMoves(
                      antChainStart,
                      antChainStart,
                      antsLeftAfterThis,
                      cacheEnabled,
                      map
                    )
                    antChainStart = originalChainStart
                    subSum
                  } else {
                    countSubMoves(
                      antChainStart,
                      antChainStart,
                      antsLeftAfterThis,
                      false,
                      map
                    )
                  }
                } else {
                  countSubMoves(
                    nextPosition,
                    start,
                    antsLeftAfterThis,
                    false,
                    map
                  )
                }
              }

              map.grid(nextPosition.x)(nextPosition.y) = false
              validSetups
            }.sum
          }

          evalOptions

        }
      }
      countSubMoves(
        antChainStart,
        antChainStart,
        nx * ny,
        true,
        MapData(taken, Dimensions(nx, ny))
      )
      //solutions
    }

    measured {
      2 to 6 foreach { x =>
        2 to 6 foreach { y =>
          countValidSetups(x,y)
        }
      }
    }
    val solution =  {
      countValidSetups(6, 6)
    }
    println(solution)
    println(cacheHits)
    println(pathCountCache.size)
  }
}
