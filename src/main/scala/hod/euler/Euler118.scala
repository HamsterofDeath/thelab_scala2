package hod.euler

import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable

object Euler118 {
  def main(args: Array[String]): Unit = {
    println("Filtering prime sets...")
    val relevant = measured {
      allPrimes
        .takeWhile(_ <= 999999999)
        .filter { e =>
          val check = e.allDigitsReversed.toList
          check == check.distinct && !check.contains(0)
        }
        .toList
    }

    println(s"${relevant.size} found")

    val groups =
      relevant.groupBy(_.allDigitsReversed.size).withDefaultValue(List.empty)

    println("Counting...")

    val distinct = mutable.HashSet.empty[Set[Int]]
    def recur(digitsCollected: BitSet, path: List[Int]): Unit = {
      def eval: Unit = {
        val free = 9 - digitsCollected.size
        if (free > 0) {
          (1 to free).foreach { take =>
            groups(take).iterator
              .filter(_.allDigitsReversed.forall(!digitsCollected(_)))
              .foreach { prime =>
                recur(digitsCollected ++ prime.allDigitsReversed, prime :: path)
              }
          }
        } else {
          distinct += path.toSet
        }
      }
      eval
    }

    measured {
      recur(BitSet.empty, List.empty)
    }
    println(distinct.size)
  }

}
