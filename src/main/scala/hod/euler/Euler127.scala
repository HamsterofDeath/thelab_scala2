package hod.euler

import java.util.concurrent.{Executors, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.collection.parallel.CollectionConverters.seqIsParallelizable
import scala.util.Random

object Euler127 {
  def main(args: Array[String]): Unit = {
    val factors = collection.mutable.HashMap.empty[Int, Set[Long]]
    def fastFactors(n: Int) = factors(n)

    def fastRad(a: Int, b: Int, c: Int) = {
      val bits = collection.mutable.HashSet.empty[Long]
      bits ++= fastFactors(a)
      bits ++= fastFactors(b)
      bits ++= fastFactors(c)
      bits.product
    }
    def gcd(a: Int, b: Int) = gcdEuclid(a, b)
    def isAbcHit(a: Int, b: Int, c: Int) = {
      val abCondition = a < b && a + b == c
      def gcdMatch = {
        gcd(a, b) == 1 &&
        gcd(a, c) == 1 &&
        gcd(b, c) == 1

      }
      def radCondition = fastRad(a, b, c) < c

      val hits = abCondition && gcdMatch && radCondition
      hits
    }

    val limit = 120000
    //val limit = 1000
    println("Precalc factors")
    measured {
      Random
        .shuffle((1 until limit).toList)
        .par
        .map { n =>
          n -> primeFactorsOf(n).toSet
        }
        .toList
        .foreach { case (n, fs) => factors.put(n, fs) }
    }
    println("Filtering")
    val counter = new AtomicInteger()
    val sum = new AtomicLong()
    val ex =
      Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
          Iterator
            .from(1)
            .takeWhile(_ < limit)
            .toList
            .reverse
        .foreach { c =>
          ex.submit {
            new Runnable {
              override def run(): Unit = {
                if (counter.incrementAndGet()%1000==0) print('.')
                val calculated = Iterator
                  .from(1)
                  .take(c)
                  .map { a =>
                    (a, c - a, c)
                  }
                  .filter { case (a, b, c) => isAbcHit(a, b, c) }
                  .map(_._3.toLong)
                  .sum
                sum.addAndGet(calculated)
              }
            }
          }
        }
    ex.shutdown()
    measured {
      ex.awaitTermination(Long.MaxValue, TimeUnit.DAYS)
    }
    println(sum.get())
//    println(fastRad(115,263,9967))
//    println(slowRad(115,263,9967))
  }
}
