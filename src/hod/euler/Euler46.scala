package hod.euler

import scala.collection.mutable

object Euler46 {

  def main(args: Array[String]): Unit = {
    val tries = 650
    val primes = Stream.from(1, 2).filter(_.isPrime)
    val squareDoubles = Stream.from(1).map(e => e * e * 2)
    val limit = tries * tries

    val found = mutable.HashSet.empty[Int]
    1 to tries foreach { until =>
      val a = primes(until)
      squareDoubles.take(tries).foreach { b =>
        found += (a + b)
      }
    }

    val missing = Range(1, limit, 2).filterNot(_.isPrime).filterNot(found)
    println(missing.toList.min)

  }
}
