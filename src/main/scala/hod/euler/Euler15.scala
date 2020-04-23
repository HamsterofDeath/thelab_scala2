package hod.euler

import scala.collection.mutable

object Euler15 {
  def main(args: Array[String]): Unit = {
    val cheat = mutable.HashMap.empty[(Int, Int), Long]

    def pathSum(startHeight: Int, movesLeft: Int): Long = {
      cheat.getOrElseUpdate((startHeight, movesLeft), {
        if (movesLeft == 0)
          1
        else {
          startHeight
            .to(1, -1)
            .map { e =>
              val left = movesLeft - 1
              pathSum(e, left)
            }
            .sum
        }
      })
    }

    def calculatorForGridSize(n: Int) = pathSum(n + 1, n)

    1 to 20 foreach { n =>
      val result = calculatorForGridSize(n)
      println(result)
    }
  }
}
