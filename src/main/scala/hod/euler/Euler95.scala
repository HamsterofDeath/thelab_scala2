package hod.euler

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

object Euler95 {
  def main(args: Array[String]): Unit = {
    val cache = mutable.HashMap.empty[Int, Int]
    def sumOfProperDivisors(n: Int) = {
      cache.getOrElseUpdate(n, properDivisorsOf(n).sum.toInt)
    }
    val limit = 1000000
    val knownChain = mutable.HashMap.empty[Int, List[Int]]

    def amicableChain(n: Int) = {
      def eval = {
        val seen = mutable.BitSet.empty

        var chain = List(n)
        var next = sumOfProperDivisors(n)
        while (!seen(next) && next <= limit) {
          seen += next
          chain = next :: chain
          next = sumOfProperDivisors(next)
        }
        if (next <= limit && chain.head == chain.last) {
          chain.foreach { e =>
            knownChain += ((e, chain))
          }
          chain.reverse
        } else {
          Nil
        }
      }

      knownChain.getOrElse(n, eval)
    }

    val progress = new AtomicInteger()
    val solution = {
      Iterator
        .from(1)
        .takeWhile(_ <= limit)
        .toList
        .flatMap { start =>
          val state = progress.incrementAndGet()
          if (state % 100000 == 0) {
            println(state)
          }
          val chain = amicableChain(start)
          if (chain.forall(_ <= limit)) {
            Some(chain)
          } else {
            None
          }
        }
        .maxBy(_.size)
        .min
    }

    println(s"Solution: $solution")
  }
}
