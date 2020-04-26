package hod.euler

import scala.collection.mutable

object Euler74 {
  def main(args: Array[String]): Unit = {
    def chain(start: Long) = {
      def next(n: Long) = {
        var number = n
        var sum = 0L
        while ( number > 0) {
          val digit = number % 10
          sum += (digit match {
            case 0 => 1
            case 1 => 1
            case 2 => 2
            case 3 => 6
            case 4 => 24
            case 5 => 120
            case 6 => 720
            case 7 => 5040
            case 8 => 40320
            case 9 => 362880
          })
          number = number / 10
        }
        sum
      }
      val seen = mutable.HashSet.empty[Long]
      seen += start
      var current = start
      Iterator.single(start) ++ Iterator
        .continually {
          val ret = next(current)
          current = ret
          ret
        }
        .takeWhile { e =>
          val moveOn = !seen(e)
          seen += e
          moveOn
        }
    }

    def nonRepeatingTermCount(n: Int) = {
      chain(n).size
    }

    val solution = measured {
      (1 to 999999).iterator
        .count { e =>
          nonRepeatingTermCount(e) == 60
        }
    }
    println(solution)

  }
}
