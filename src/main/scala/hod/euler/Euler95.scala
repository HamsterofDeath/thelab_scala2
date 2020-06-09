package hod.euler

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.CollectionConverters._

object Euler95 {
  def main(args: Array[String]): Unit = {
    val cache = mutable.HashMap.empty[Int, Int]
    def sumOfProperDivisors(n: Int) = {
      cache.getOrElseUpdate(n, properDivisorsOf(n).sum)
    }
    val limit = 1000000
    val knownChain = mutable.HashMap.empty[Int, List[Int]]

    def amicableChain(n: Int) = {
      def eval = {
        val seen = mutable.BitSet.empty

        def isKnown(x: Int) = seen(x)

        if (isKnown(n)) {
          Nil
        } else {
          var chain = List(n)
          var next = sumOfProperDivisors(n)
          while (!isKnown(next) && next <= limit) {
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

      }

      knownChain.getOrElse(n, eval)
    }

    val progress = new AtomicInteger()
    var largest = List.empty[Int]
    val solution = Iterator
      .from(1)
      .takeWhile(_ <= limit)
      .toList
      .flatMap { start =>
        val state = progress.incrementAndGet()
        if (state % 1000 == 0) {
          println(state)
        }
        val chain = amicableChain(start)
        if (chain.forall(_ <= limit)) {
          if (chain.size>largest.size) {
            println(chain.size)
            println(chain)
            largest = chain
          }
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
