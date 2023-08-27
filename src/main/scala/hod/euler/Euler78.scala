package hod.euler

import scala.collection._

object Euler78 {
  def main(args: Array[String]): Unit = {
    val partitionCounts = mutable.ArrayBuffer.empty[BigInt]
    partitionCounts ++= List(1, 1)
    val positions = {
      def nextPentagonalHelper(n: Int) = {
        if (n < 0) (-n) + 1 else -n
      }

      var cursor = 1
      Iterator
        .continually {
          val helper = cursor
          cursor = nextPentagonalHelper(cursor)
          helper * (3 * helper - 1) / 2
        }
        .take(100000)
        .toIndexedSeq
    }

    def evalNext = {
      val nextIndex = partitionCounts.size
      val nextNumber = {
        val relevantPositions = positions.iterator.takeWhile(_ < nextIndex)
        var sign = 1
        val element = relevantPositions
          .grouped(2)
          .map { indexes =>
            val fiddlySum = indexes
              .map(where => partitionCounts(nextIndex - where) * sign)
              .sum
            sign = -sign
            fiddlySum
          }
          .sum
        partitionCounts += element
        element
      }
      nextNumber
    }

    val solution = Iterator
      .continually(evalNext)
      .find(_ % 1000000 == 0)
      .map(partitionCounts.indexOf)
      .map(_ - 1)
    println(solution)

  }

}
