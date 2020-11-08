package hod.training

object FillWithSingleLine {
  case class Position(x:Int, y:Int) {
    def translated(direction: Position): Position = Position(x+direction.x, y+direction.y)

  }

  def main(args: Array[String]): Unit = {
    val level =
      s"""|BBBBBBBBBBBBB
          |B           B
          |B B         B
          |B B         B
          |BSB         B
          |BBBBBBBBBBBBB
          |""".stripMargin

    var start:Position = null
    val sizeX = level.linesIterator.next.length
    val sizeY = level.linesIterator.length
    val blocked = {
      val lines = level.linesIterator.toArray
      Array.tabulate[Boolean](sizeX, sizeY)((x, y) => {
        lines(y)(x) match {
          case 'B' => true
          case 'S' =>
            start = Position(x,y)
            true
          case _ => false
        }
      })
    }

    val allMoves = List(
      Position(-1,0),
      Position(1,0),
      Position(0,-1),
      Position(0,1),
    )
    var openFields = blocked.map(_.count(_ == false)).sum + 1
    def tryMove(
                 current:Position,
                 pastMoves:List[Position]
               ): Unit = {
      val solutionFound = openFields == 0
      if (solutionFound) {
        val asDirections = {
          (pastMoves.sliding(2,1).map { case Seq(from, to) =>
            val difference = Position(to.x-from.x, to.y-from.y)
            difference match {
              case Position(1,0) => "→"
              case Position(0,1) => "↓"
              case Position(0,-1) => "↑"
              case Position(-1,0) => "←"
            }
          } ++ Iterator.single("Z")).toList
        }

        val moveAtPosition = pastMoves.zip(asDirections).toMap

        val solutionExplained = {
          Array.tabulate[String](sizeY, sizeX)((y, x) => {
            val where = Position(x, y)
            if (where==start) "S" else moveAtPosition.getOrElse(where, "B")
          }).map(_.mkString)
        }.mkString("\n")
        println(s"Solution:\n$solutionExplained")
      } else {
        def wouldCover(direction:Position) = {
          def notBlocked(p:Position) = {
            p.x >=0 &&
              p.y >=0 &&
              p.x < sizeX &&
              p.y < sizeY &&
              !blocked(p.x)(p.y)
          }
          Iterator.iterate(current)(_.translated(direction))
            .takeWhile { p =>
              p == current || notBlocked(p)
            }.toList
        }

        allMoves.foreach { direction =>
          val nextLine = wouldCover(direction)
          if (nextLine.sizeCompare(1) == 1) {
            nextLine.foreach { p =>
              blocked(p.x)(p.y) = true
            }
            val isInitial = current == start
            val addedSteps = {
              nextLine.size - (if (isInitial) 0 else 1)
            }
            val mergedMoves = {
              pastMoves ++ (if (isInitial) {
                nextLine
              } else {
                nextLine.tail
              })
            }
            openFields -= addedSteps

            tryMove(nextLine.last, mergedMoves)
            openFields -= addedSteps
            nextLine.foreach { p =>
              blocked(p.x)(p.y) = false
            }
          }
        }

      }
    }
    tryMove(start, Nil)
  }
}
