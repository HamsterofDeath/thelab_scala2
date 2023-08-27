package hod.euler

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import java.util.concurrent.{Executors, TimeUnit}
import scala.collection.parallel.CollectionConverters.seqIsParallelizable
import scala.util.Random

object Euler127 {
  def main(args: Array[String]): Unit = {
    val rads = collection.mutable.HashMap.empty[Int, Long]

    def fastRad(n: Int) = {
      rads(n)
    }
    def veryFastRad(a: Int, b: Int, c: Int) = {
      fastRad(a) * fastRad(b) * fastRad(c)
    }
    def gcd(a: Int, b: Int) = gcdEuclid(a, b)
    def isAbcHit(a: Int, b: Int, c: Int) = {
      val abCondition = a < b && a + b == c
      def gcdMatch = {
        gcd(a, b) == 1 &&
        gcd(a, c) == 1 &&
        gcd(b, c) == 1
      }
      def radCondition = veryFastRad(a, b, c) < c

      val hits = abCondition && gcdMatch && radCondition
      hits
    }

    val limit = 120000
    //val limit = 1000
    bench("precalculate") {
      Random
        .shuffle((1 until limit).toList)
        .par
        .map { n =>
          n -> primeFactorsOf(n).toSet
        }
        .toList
        .foreach {
          case (n, fs) =>
            rads.put(n, fs.product)
        }
    }
    val counter = new AtomicInteger()
    val sum = new AtomicLong()
    val ex =
      Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors())
    bench("Jobs") {
      Iterator
        .from(1)
        .takeWhile(_ < limit)
        .toList
        .foreach { c =>
          ex.submit {
            new Runnable {
              override def run(): Unit = {
                if (counter.incrementAndGet() % 1000 == 0) print('.')
                val calculated = Iterator
                  .from(1)
                  .take(c)
                  .map { a =>
                    val b = c - a
                    if (isAbcHit(a, b, c)) c.toLong else 0L
                  }
                  .sum
                sum.addAndGet(calculated)
              }
            }
          }
        }
    }
    ex.shutdown()
    bench("Work") {
      ex.awaitTermination(Long.MaxValue, TimeUnit.DAYS)
    }
    println(sum.get())
//    println(fastRad(115,263,9967))
//    println(slowRad(115,263,9967))
  }
}
