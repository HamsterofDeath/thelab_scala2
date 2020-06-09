package hod.euler

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.CollectionConverters._

object Euler95 {
  def main(args: Array[String]): Unit = {
    def sumOfProperDivisors(n: Int) = properDivisorsOf(n).sum
    val limit = 1000000

    val seen = mutable.BitSet.empty

    def amicableChain(n: Int) = {
      def isKnown(x: Int) = seen.synchronized {
        seen(x)
      }

      if (isKnown(n)) {
        Nil
      } else {
        var chain = List(n)
        var next = sumOfProperDivisors(n)
        while (!isKnown(next) && next <= limit) {
          seen.synchronized {
            seen += next
          }

          chain = next :: chain
          next = sumOfProperDivisors(next)
        }
        if (next <= limit) {
          chain.reverse
        } else {
          Nil
        }
      }
    }

    val progress = new AtomicInteger()
    val solution = Iterator
      .from(1)
      .takeWhile(_ <= limit)
      .toList
      .reverse
      .par
      .flatMap { start =>
        val state = progress.incrementAndGet()
        if (state % 1000 == 0) {
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
    println(solution)
  }
}
