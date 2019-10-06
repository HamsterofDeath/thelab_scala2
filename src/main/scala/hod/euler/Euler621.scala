package hod.euler

import java.text.DecimalFormat
import scala.collection.parallel.CollectionConverters._

object Euler621 {

  def main(args: Array[String]): Unit = {
    val billion = 1000000000L
    val target = 17526 * billion

    def df = new DecimalFormat("#0.0000")

    def toTriNum(x: Int) = {
      (x * (x.toLong + 1)) / 2
    }

    def reverseTriNum(i: Long) = {
      // wikipedia to the rescue
      // https://en.wikipedia.org/wiki/Quadratic_formula
      // (x*x + x)/2 = i => 0.5x² + 0.5x = i => x²+x=2i => x²+x-2i = 0
      val a = 1
      val b = 1
      val c = 2 * -i
      val orig = {
        val top = -b + math.sqrt(b * b - 4 * a * c)
        val bottom = 2 * a
        top / bottom
      }
      orig
    }

    def isTriNum(i: Long) = {
      toTriNum(reverseTriNum(i).toInt) == i
    }

    def findMaxTriNumberIndexFor(i: Long) = {
      val rev = reverseTriNum(i)
      val max = rev.toInt
      max
    }

    def solveFor(maxLimit: Long): Unit = {
      val start = System.nanoTime()

      def countSolutions = {
        def subSolutionCount(sum: Long) = {
          val maxIndex = findMaxTriNumberIndexFor(sum)
          var oldSchool = maxIndex
          var count = 0
          while (oldSchool >= 0) {
            val asTri = toTriNum(oldSchool)
            val remainder = sum - asTri
            if (isTriNum(remainder)) {
              count += 1
            }
            oldSchool -= 1
          }
          count
        }

        val maxIndex = findMaxTriNumberIndexFor(maxLimit)
        @volatile var progress = 0
        (0 to maxIndex).par.map { i =>
          progress += 1
          if (progress % 100 == 0) {
            val elapsed = s"${df.format((System.nanoTime() - start) / 1000000000.0 / 60)} min"
            println(s"$progress / $maxIndex, ${df.format(progress / maxIndex.toDouble)}% in $elapsed")
          }
          val asTri = toTriNum(i)
          val remainder = maxLimit - asTri
          subSolutionCount(remainder)
        }.sum
      }

      val solution = countSolutions
      val end = System.nanoTime()
      println(s"Solutions = $solution in ${df.format((end - start) / 1000000000.0 / 60)} min")
    }

    //    solveFor(1000000)
    solveFor(target)
  }
}


