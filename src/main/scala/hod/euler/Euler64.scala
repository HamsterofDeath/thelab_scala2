package hod.euler

import scala.collection.parallel.CollectionConverters._

object Euler64 {
  def main(args: Array[String]): Unit = {
    val count = {
      (1 to 10000).par
        .filterNot(_.isPerfectSquare)
        .map { n =>
          val fractions = {
            n.sqrtPrecise(2500)
             .continuedFractions
             .memoizedByIndex
          }

          val checkCount = 10
          val lengthOfPeriod = {
            Iterator
              .from(1)
              .take(250)
              .find { length =>
                val samples = {
                  Iterator
                    .from(1)
                    .map(fractions)
                    .grouped(length)
                    .take(checkCount)
                    .toList
                }
                val good    = {
                  samples.distinct.size == 1
                }
                if (good) {
                  println(
                    s"found period for n $n = ${
                      if (samples.head.size % 2 == 0) "even"
                      else "odd"
                    }"
                  )
                }
                good
              }
              .openOr("solution must exist")
          }
          lengthOfPeriod
        }
                  .count(_ % 2 != 0)
    }
    println(count)
  }
}
