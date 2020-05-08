package hod.euler

object Euler393 {
  def main(args: Array[String]): Unit = {

    def countValidSetups(nx: Int, ny: Int) = {
      case class Position(x: Int, y: Int) {
        def adjacent = {
          var valid = List.empty[Position]
          if (x > 0) {
            valid = Position(x-1,y) :: valid
          }
          if (x < nx-1) {
            valid = Position(x+1,y) :: valid
          }
          if (y > 0) {
            valid = Position(x,y-1) :: valid
          }
          if (y < ny-1) {
            valid = Position(x,y+1) :: valid
          }
          valid
        }
      }
      val taken = Array.fill[Boolean](nx, ny)(false)

      def anyUnprocessedPosition: Position = {
        0 until nx foreach { x =>
          0 until ny foreach { y =>
            if (!taken(x)(y))
              return Position(x, y)
          }
        }
        throw new IllegalStateException()
      }

      var antChainStart = Position(0, 0)

      def countSubMoves(start: Position,
                        blocked: Position,
                        antsLeft: Int): Int = {
        if (antsLeft == 0) {
//          require(taken.forall(_.forall(_ == true)))
//          require(start == antChainStart, s"$start != $antChainStart")
          1
        } else {
          val moves = start.adjacent
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
                    antsLeftAfterThis
                  )
                  antChainStart = originalChainStart
                  subSum
                } else {
                  countSubMoves(antChainStart, antChainStart, antsLeftAfterThis)
                }
              } else {
                countSubMoves(nextPosition, start, antsLeftAfterThis)
              }

            taken(nextPosition.x)(nextPosition.y) = false
            validSetups
          }.sum
        }
      }
      countSubMoves(antChainStart, antChainStart, nx * ny)
    }

    val solution = measured{
      countValidSetups(6,6)
    }
    println(solution)
  }
}
