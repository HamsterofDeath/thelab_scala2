package hod.euler

import scala.collection.mutable

object Euler111 {
  def main(args: Array[String]): Unit = {
    val targetDigits = 10

    val min = "".padTo(targetDigits - 1, '9').toLong
    val max = "".padTo(targetDigits, '9').toLong

    val withTenDigits: Iterator[Long] = {
      allPrimesLong.filter(_ > min).takeWhile(_ <= max)
    }

    class Primes(val targetDigit: Int) {
      var highestCount = 0
      var matchCount = 0
      val matches      = mutable.HashSet.empty[Long]
      var sum = 0L

      def initWith(prime: Long) = {
        highestCount = prime.toString.count(_.asDigit == targetDigit)
        matches.clear()
        matchCount = 0
        sum = 0
        add(prime)
      }

      def add(prime: Long) = {
        matches += prime
        matchCount += 1
        sum += prime
      }

      override def toString = s"Primes($highestCount, $matchCount, $sum, $targetDigit)"
    }
    val analyzed = mutable.HashMap.empty[Int, Primes]
    withTenDigits.foreach { prime =>
      val occurences = prime.toString.map(_.asDigit).occurences
      occurences.foreach { case (digit, count) =>
        analyzed.get(digit) match {
          case Some(subPrimes) if subPrimes.highestCount == count =>
            subPrimes.add(prime)
          case Some(subPrimes) if subPrimes.highestCount > count =>
          case Some(subPrimes) if subPrimes.highestCount < count =>
            subPrimes.initWith(prime)
          case None =>
            val primes = new Primes(digit)
            primes.initWith(prime)
            analyzed.put(digit, primes)
        }
      }
    }
    val solution = analyzed.values.map(_.sum).sum
    println(solution)
  }
}
