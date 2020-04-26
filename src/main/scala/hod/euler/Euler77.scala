package hod.euler

import scala.collection.mutable

object Euler77 {
  def main(args: Array[String]): Unit = {
    val cache = mutable.HashMap.empty[(Int, Int), Int]
    def primeSummations(n: Int, maxUsed: Int): Int = {
      def eval = {

        if (n == 0) {
          1
        } else {
          val allPossibleParticles = {
            allPrimes
              .filter(_ >= maxUsed)
              .takeWhile(_ <= n)
          }

          allPossibleParticles.map { particle =>
            val newN = n - particle
            primeSummations(newN, maxUsed.max(particle))
          }.sum
        }
      }
      cache.getOrElseUpdate((n, maxUsed), eval)
    }

    def solveFor(n: Int) = primeSummations(n, 0)

    val solution = {
      measured {
        Iterator
          .from(1)
          .map { e =>
            e -> solveFor(e)
          }
          .find(_._2 > 5000)
          .map(_._1)
      }
    }

    println(s"Solution: ${solution.get}")

  }
}
