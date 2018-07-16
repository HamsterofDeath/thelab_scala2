package hod.euler

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

object Euler60 {
  def main(args: Array[String]): Unit = {
    val primeCount = 3000
    val combinations = 5

    def somePrimes = primes(primeCount).toList

    def findNextValidPrimeToAdd(already: Seq[Int]) = {
      somePrimes.find { candidate =>
        already.forall { old =>
          s"$candidate$old".toLong.isPrime &&
          s"$old$candidate".toLong.isPrime
        }
      }
    }

    somePrimes.foreach { origin =>
      val workingSet = ArrayBuffer.empty[Int] += origin
      var skipToNextSolution = false

      def goOn = !skipToNextSolution && workingSet.size < combinations

      while (goOn) {
        findNextValidPrimeToAdd(workingSet) match {
          case None =>
            skipToNextSolution = true
          case Some(good) =>
            workingSet += good
            if (workingSet.size == combinations) {
              println(s"${workingSet.sorted} => ${workingSet.sum}")
              skipToNextSolution = true
            }
        }
      }
    }
  }
}
