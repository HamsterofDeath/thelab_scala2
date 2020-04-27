package hod.euler

import scala.collection.mutable
import scala.collection.parallel._

object Euler178 {
  private val cache = mutable.HashMap.empty[(Int, Int, Int), Long]

  def numberSnakeCounts(n: Int) = {
    var cacheHit = 0L
    var cacheMiss = 0L
    def snakedySnake(remaining: Int, lastStep: Int, seenDigits: Int): Long = {
      val seenDigitCount = Integer.bitCount(seenDigits)
      def recur: Long = {
        val end = remaining == 0
        val missingDigits = 10 - seenDigitCount
        val impossible = remaining < missingDigits
        if (impossible) {
          0L
        } else if (end) {
          if (seenDigitCount == 10) 1L else 0L
        } else {
          def sumForNextN(next: Int) = {
            snakedySnake(remaining - 1, next, seenDigits | 1 << next)
          }
          lastStep match {
            case 0 => sumForNextN(1)
            case 9 => sumForNextN(8)
            case _ => sumForNextN(lastStep - 1) + sumForNextN(lastStep + 1)
          }
        }
      }

      cache.synchronized {
        cacheHit += 1
        val state = (remaining, lastStep, seenDigits)
        cache.getOrElseUpdate(state, {
          cacheMiss += 1
          cacheHit -= 1
          recur
        })
      }
    }

    val sum = (1 to 9).map { first =>
      snakedySnake(n - 1, first, 1 << first)
    }.sum

    sum
  }

  def main(args: Array[String]): Unit = {
    val total = measured {
      (1 to 40).map { n =>
        numberSnakeCounts(n)
      }.sum

    }
    println(total)

  }

}
