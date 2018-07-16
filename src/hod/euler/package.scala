package hod

import scala.collection.mutable

package object euler {

  def allPrimes = {
    Iterator(2, 3) ++ Iterator.from(5, 2).filter(_.isPrime)
  }

  def primes(n:Int) = {
    allPrimes.take(n)
  }

  implicit class IntOps(val i: Int) extends AnyVal {

    def isPrime: Boolean = {
      if (i % 2 == 0) return i == 2
      if (i % 3 == 0) return i == 3
      val sqrtn = Math.sqrt(i)
      var j = 5
      var step = 4
      while (j <= sqrtn) {
        if (i % j == 0) return false
        step = 6 - step
        j += step
      }
      true
    }
  }

  implicit class LongOps(val i: Long) extends AnyVal {

    def isPrime: Boolean = {
      if (i % 2 == 0) return i == 2
      if (i % 3 == 0) return i == 3
      val sqrtn = Math.sqrt(i)
      var j = 5
      var step = 4
      while (j <= sqrtn) {
        if (i % j == 0) return false
        step = 6 - step
        j += step
      }
      true
    }
  }

}
