package hod.euler

import scala.collection.{BitSet, mutable}
import scala.io.AnsiColor

object Euler393 {
  case class Position(x: Int, y: Int) {
    def isExactlyBetween(a: Position, b: Position): Boolean = {
      if (a.x == b.x && a.x == x) {
        a.y + 1 == y && y + 1 == b.y ||
        b.y + 1 == y && y + 1 == a.y
      } else if (a.x == b.x && a.x == x) {
        a.x + 1 == x && x + 1 == b.x ||
        b.x + 1 == x && x + 1 == a.x
      } else {
        false
      }

    }

  }
  case class Move(from: Position, to: Position)
  case class Dimensions(x: Int, y: Int)
  case class ShapeAndStartingPoint(mapAsBitSet: BitSet, context: Context)
  case class Context(a: Position,
                     b: Position,
                     c: Position,
                     d: Position,
                     left: Int)
  case class Shape(dimensions: Dimensions, data: BitSet)(
    val upLeft: Position,
    toPositition: (Int, Int) => Position
  ) {
    def canBeValid: Boolean = {
      data.size % 2 == 0
    }

    def allPositions: Iterator[Position] = data.iterator.map { index =>
      toPositition(index % dimensions.x, index / dimensions.x)

    }
    def anyPosition: Position = {
      val index = data.head
      toPositition(index % dimensions.x, index / dimensions.x)
    }

    def shapeSize: Int = data.size

    def contains(x: Int, y: Int): Boolean = data.contains(x + y * dimensions.x)

  }

  case class MapData(dimensions: Dimensions) {
    def batchFill(filled:(Int, Int)=>Boolean): MapData = {
      0 until dimensions.x foreach { x =>
        0 until dimensions.y foreach { y =>
          if (filled(x, y)) {
            grid += bitOf(x, y)
          }
        }
      }
      this
    }

    def nx: Int = dimensions.x
    def ny: Int = dimensions.y

    private val grid = mutable.BitSet.empty

    private val positions: Array[Array[Position]] =
      Array.tabulate(nx, ny)((nx, ny) => Position(nx, ny))

    def adjacent(pos: Position): List[Position] = {
      var valid = List.empty[Position]
      if (pos.x > 0) {
        valid = positions(pos.x - 1)(pos.y) :: valid
      }
      if (pos.x < dimensions.x - 1) {
        valid = positions(pos.x + 1)(pos.y) :: valid
      }
      if (pos.y > 0) {
        valid = positions(pos.x)(pos.y - 1) :: valid
      }
      if (pos.y < dimensions.y - 1) {
        valid = positions(pos.x)(pos.y + 1) :: valid
      }
      valid
    }

    def determineShapeAt(current: Position): Shape = {
      val area = mutable.BitSet.empty

      def posToIndex(p: Position) = p.x + p.y * dimensions.x
      var minX, minY = Integer.MAX_VALUE
      var maxX, maxY = Integer.MIN_VALUE
      def floodFill(current: Position): Unit = {
        area += posToIndex(current)
        minX = minX min current.x
        minY = minY min current.y
        maxX = maxX max current.x
        maxY = maxY max current.y
        adjacent(current).foreach { ad =>
          if (isFree(ad) && !area.contains(posToIndex(ad))) {
            floodFill(ad)
          }
        }
      }
      floodFill(current)
      val size = Dimensions(maxX - minX + 1, maxY - minY + 1)

      val locations = area.map { index =>
        val loc =
          positions(index % dimensions.x)(index / dimensions.x)
        val relativeX = loc.x - minX
        val relativeY = loc.y - minY
        relativeX + relativeY * size.x
      }
      Shape(size, locations)(positions(minX)(minY), (x, y) => positions(x)(y))

    }

    def setBlocked(x: Int, y: Int): Unit = {
      grid += bitOf(x, y)
    }
    private def bitOf(x: Int, y: Int) = {
      x + y * nx
    }
    def setBlocked(pos: Position): Unit = {
      grid += bitOf(pos.x, pos.y)
    }
    def setFree(x: Int, y: Int): Unit = {
      grid -= bitOf(x,y)
    }
    def setFree(pos: Position): Unit = {
      grid -= bitOf(pos.x,pos.y)
    }

    def isBlocked(pos: Position): Boolean = {
      grid(bitOf(pos.x,pos.y))
    }
    def isFree(pos: Position): Boolean = {
      !grid(bitOf(pos.x,pos.y))
    }
    def isFree(x: Int, y: Int): Boolean = {
      !grid(bitOf(x,y))
    }

    override def toString: String = {
      (0 until dimensions.y)
        .map { y =>
          (0 until dimensions.x).map { x =>
            if (grid(x + nx*(dimensions.y - y - 1))) "X" else "+"
          }.mkString
        }
        .mkString("\n")
    }
  }

  def toString(moves: List[Move]): String = {
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

    val areaScoreCache = mutable.HashMap.empty[Shape, Long]
    var cacheHitsArea = 0L
    var deadEndCuts = 0L

    val areaCacheEnabled = true
    val deadEndCutEnabled = true

    def countValidSetups(nx: Int, ny: Int): Long = {

      val positions = Array.tabulate(nx, ny)((nx, ny) => Position(nx, ny))

      def posAt(x: Int, y: Int) = positions(x)(y)

      def countSubMoves(current: Position,
                        previous: Position,
                        chainStart: Position,
                        antsLeft: Int,
                        canCacheOnThisLevel: Boolean,
                        map: MapData): Long = {
        def anyUnprocessedPosition: Position = {
          0 until map.dimensions.x foreach { x =>
            0 until map.dimensions.y foreach { y =>
              if (map.isFree(x, y))
                return posAt(x, y)
            }
          }
          throw new IllegalStateException()
        }

        if (antsLeft == 0) {
          1L
        } else if (canCacheOnThisLevel && areaCacheEnabled) {
          val shape = {
            map.determineShapeAt(current)
          }

          cacheHitsArea += 1

          val intermediateSum = {
            areaScoreCache.getOrElseUpdate(
              shape, {
                cacheHitsArea -= 1
                val needsEvaluation = {
                  shape.dimensions.x > 1 &&
                  shape.dimensions.y > 1
                }
                if (needsEvaluation) {
                  val miniMap = {
                    MapData(
                      shape.dimensions
                    ).batchFill ((x,y) => {
                      !shape.contains(x, y)
                    })
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
          }

          shape.allPositions.foreach { loc =>
            map.setBlocked(loc.x + shape.upLeft.x, loc.y + shape.upLeft.y)
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
            map.setFree(loc.x + shape.upLeft.x, loc.y + shape.upLeft.y)
          }
          ret
        } else {
          val moves = {
            map
              .adjacent(current)
              .filter { p =>
                p != previous && map.isFree(p)
              }
          }

          def countScoreOf(nextPosition: Position): Long = {
            map.setBlocked(nextPosition)
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
                    areaCacheEnabled,
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
                  def producesDeadEnd = {
                    deadEndCutEnabled &&
                    current != nextPosition &&
                    map.adjacent(current).exists { checkIfLockedIn =>
                      if (checkIfLockedIn == chainStart) {
                        nextPosition == chainStart
                      } else if (map.isBlocked(checkIfLockedIn)) {
                        false
                      } else {
                        val next = map.adjacent(checkIfLockedIn)
                        val free = next.count { adad =>
                          map.isFree(adad)
                        }
                        free <= 1
                      }
                    }
                  }
                  if (producesDeadEnd) {
                    deadEndCuts += 1
                    true
                  } else {
                    false
                  }
                }
                if (invalid) {
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

            map.setFree(nextPosition)
            validSetups

          }
          var sum = 0L
          moves.foreach { nextPosition =>
            sum += countScoreOf(nextPosition)
          }
          sum
        }
      }

      val defaultStart = positions(0)(0)
      countSubMoves(
        defaultStart,
        defaultStart,
        defaultStart,
        nx * ny,
        true,
        MapData(Dimensions(nx, ny))
      )
    }

    val solution =
      measured {
        countValidSetups(6, 6)
      }
    println(s"Solution: $solution")
    println(s"Cuts (dead end): $deadEndCuts")
    println(s"Cache hits (area): $cacheHitsArea")
    println(s"Cache size (area): ${areaScoreCache.size}")
  }
}
