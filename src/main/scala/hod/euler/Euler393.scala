package hod.euler

import scala.collection.{BitSet, mutable}
import scala.io.AnsiColor

object Euler393 {
  case class Position(x: Int, y: Int)
  case class Move(from: Position, to: Position)
  case class Dimensions(x: Int, y: Int)
  case class Shape(dimensions: Dimensions, data: BitSet)(
    val upLeft: Position,
    toPositition: (Int, Int) => Position
  ) {

    def allPositions = data.iterator.map { index =>
      toPositition(index % dimensions.x, index / dimensions.x)

    }
    def anyPosition = {
      val index = data.head
      toPositition(index % dimensions.x, index / dimensions.x)
    }

    def shapeSize: Int = data.size

    def contains(x: Int, y: Int) = data.contains(x + y * dimensions.x)

  }
  case class MapData(grid: Array[Array[Boolean]], dimensions: Dimensions) {
    override def toString = {
      (0 until dimensions.y)
        .map { y =>
          (0 until dimensions.x).map { x =>
            if (grid(x)(dimensions.y - y - 1)) "X" else "+"
          }.mkString
        }
        .mkString("\n")
    }
  }

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
    var cutOff = 0
    def countValidSetups(nx: Int, ny: Int): Long = {

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

      val cacheEnabled = true
      val cutEnabled = true
      def countSubMoves(current: Position,
                        previous: Position,
                        chainStart: Position,
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
            val area = mutable.BitSet.empty

            def posToIndex(p: Position) = p.x + p.y * map.dimensions.x
            var minX, minY = Integer.MAX_VALUE
            var maxX, maxY = Integer.MIN_VALUE
            def floodFill(current: Position): Unit = {
              area += posToIndex(current)
              minX = minX min current.x
              minY = minY min current.y
              maxX = maxX max current.x
              maxY = maxY max current.y
              adjacent(current, map.dimensions).foreach { ad =>
                if (!map.grid(ad.x)(ad.y) && !area.contains(posToIndex(ad))) {
                  floodFill(ad)
                }
              }
            }
            floodFill(current)
            val size = Dimensions(maxX - minX + 1, maxY - minY + 1)

            val locations = area.map { index =>
              val loc =
                positions(index % map.dimensions.x)(index / map.dimensions.x)
              val relativeX = loc.x - minX
              val relativeY = loc.y - minY
              relativeX + relativeY * size.x
            }
            Shape(size, locations)(
              positions(minX)(minY),
              (x, y) => positions(x)(y)
            )
          }

          cacheHits += 1

          val intermediateSum =
            pathCountCache.getOrElseUpdate(
              shape, {
                cacheHits -= 1
                val needsEvaluation = {
                  shape.dimensions.x > 1 &&
                  shape.dimensions.y > 1
                }
                if (needsEvaluation) {
                  val miniMap = {
                    MapData(
                      Array.tabulate(shape.dimensions.x, shape.dimensions.y)(
                        (x, y) => !shape.contains(x, y)
                      ),
                      shape.dimensions
                    )
                  }
                  val startAt = shape.anyPosition
                  val subSum = countSubMoves(
                    startAt,
                    startAt,
                    shape.anyPosition,
                    shape.shapeSize,
                    false,
                    miniMap
                  )
                  subSum
                } else {
                  0
                }
              }
            )

          shape.allPositions.foreach { loc =>
            map.grid(loc.x + shape.upLeft.x)(loc.y + shape.upLeft.y) = true
          }

          val ret = {
            if (antsLeft - shape.shapeSize > 0) {
              if (intermediateSum > 0) {
                val nextStart = anyUnprocessedPosition
                intermediateSum * countSubMoves(
                  nextStart,
                  nextStart,
                  chainStart,
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
          shape.allPositions.foreach { loc =>
            map.grid(loc.x + shape.upLeft.x)(loc.y + shape.upLeft.y) = false
          }
          ret
        } else {
          val moves = {
            adjacent(current, map.dimensions)
              .filter { p =>
                p != previous && !map.grid(p.x)(p.y)
              }
          }

          moves.map { nextPosition =>
            map.grid(nextPosition.x)(nextPosition.y) = true
            val isChainComplete = nextPosition == chainStart
            val antsLeftAfterThis = antsLeft - 1
            val validSetups = {
              if (isChainComplete) {
                if (antsLeftAfterThis > 0) {
                  val nextStart = anyUnprocessedPosition
                  val subSum = countSubMoves(
                    nextStart,
                    nextStart,
                    nextStart,
                    antsLeftAfterThis,
                    cacheEnabled,
                    map
                  )
                  subSum
                } else {
                  countSubMoves(
                    chainStart,
                    chainStart,
                    chainStart,
                    antsLeftAfterThis,
                    false,
                    map
                  )
                }
              } else {
                val invalid = {
                  cutEnabled &&
                  current != nextPosition &&
                  adjacent(current, map.dimensions).exists { checkIfLockedIn =>
                    if (checkIfLockedIn == chainStart) {
                      nextPosition == chainStart
                    } else if (map.grid(checkIfLockedIn.x)(checkIfLockedIn.y)) {
                      false
                    } else {
                      val next = adjacent(checkIfLockedIn, map.dimensions)
                      val free = next.count { adad =>
                        !map.grid(adad.x)(adad.y)
                      }
                      free <= 1
                    }
                  }
                }
                if (invalid) {
                  cutOff += 1
                  0
                } else {
                  countSubMoves(
                    nextPosition,
                    current,
                    chainStart,
                    antsLeftAfterThis,
                    false,
                    map
                  )
                }
              }
            }

            map.grid(nextPosition.x)(nextPosition.y) = false
            validSetups
          }.sum
        }
      }

      val defaultStart = positions(0)(0)
      countSubMoves(
        defaultStart,
        defaultStart,
        defaultStart,
        nx * ny,
        true,
        MapData(taken, Dimensions(nx, ny))
      )
    }

    val solution =
      measured {
        countValidSetups(6, 6)
      }
    println(s"Solution: $solution")
    println(s"Cuts: $cutOff")
    println(s"Cache hits: $cacheHits")
    println(s"Cache size: ${pathCountCache.size}")

  }
}
