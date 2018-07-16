package hod.euler

import scala.collection.immutable.BitSet
import scala.collection.{immutable, mutable}

object Euler51 {
  def main(args: Array[String]): Unit = {

    def primesWithNDigits(n: Int) = {
      val min = math.pow(10, n - 1).toInt
      val max = math.pow(10, n).toInt
      allPrimes
      .dropWhile(_ <= min)
      .takeWhile(_ < max)
    }

    def solveFor(length: Int) = {
      val allPrimes = primesWithNDigits(length).toList

      val primeStrings = allPrimes.map(_.toString)
      val primeStringsWithIndex = primeStrings.map(_.zipWithIndex)
      val maskToSeenChars = {
        val fill = mutable.HashMap.empty[BitSet, mutable.TreeSet[Char]]
        primeStringsWithIndex.foreach { prime =>
          '0' to '9' foreach { maskDigit =>
            val mask = {
              prime
              .filter(_._1 == maskDigit)
              .map(_._2)
            }
            if (mask.nonEmpty) {
              val key = immutable.BitSet(mask: _*)
              fill.getOrElseUpdate(key, mutable.TreeSet.empty) += maskDigit
            }
          }
        }
        fill
      }

      val allByPattern = {
        val range = 0 until length

        maskToSeenChars.flatMap { case (pattern, seen) =>
          val stringsFittingToPattern = {
            primeStrings.filter { primeString =>
              val sample = primeString(pattern.head)
              pattern.forall(e => primeString(e) == sample)
            }
          }

          def toStringPattern(primeString: String) = {
            range.map { index =>
              if (pattern(index)) {
                'X'
              } else {
                primeString(index)
              }
            }.mkString
          }

          stringsFittingToPattern.groupBy(toStringPattern)
        }
      }

      def iterateOverSolutions = {
        allByPattern.iterator
      }

      val largestSize = {
        iterateOverSolutions
        .maxBy(_._2.size)
        ._2
        .size
      }
      val smallestWithLargestSize = {
        iterateOverSolutions
        .filter(_._2.size == largestSize)
        .minBy(_._2.head)
      }
      smallestWithLargestSize
    }

    val length = 6
    val largest = solveFor(length)
    println(largest)
  }
}
