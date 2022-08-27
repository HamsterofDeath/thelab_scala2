package hod.euler

import scala.collection.mutable

object Euler133 {
  def main(args: Array[String]): Unit = {
    val guess = 200
    val primes = mutable.HashSet.empty ++= allPrimes.takeWhile(_< 100000).toSet
    Iterator.from(2).take(guess).foreach { n =>
      val k = BigInt(10).pow(n)
      val exclude = primes.filter { p =>
        BigInt(10).modPow(k, 9 * p) == 1
      }
      primes --= exclude
      println(s"$n excludes $exclude,${primes.size} remain, sum ${primes.sum}")
    }
    println(primes.sum)
  }
}
