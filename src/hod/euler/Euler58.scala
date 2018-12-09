package hod.euler

object Euler58 {
  def main(args: Array[String]): Unit = {
    def sideLength(step: Int) = {
      step * 2 + 1
    }

    def edges(n: Int) = {
      val lineLength = sideLength(n)
      val lowerRight = lineLength * lineLength
      Iterator.from(lowerRight, -(lineLength - 1)).take(4)
    }

    var primes = 0
    var nonPrimes = 1
    var size = 0

    def targetRatioReached = {
      primes > 0 && primes / (nonPrimes + primes).toDouble < 0.1
    }

    while (!targetRatioReached) {
      size += 1
      edges(size).foreach { i =>
        if (i.isPrime) {
          primes += 1
        } else {
          nonPrimes += 1
        }
      }
    }

    println {
      s"""|$primes primes
          |$nonPrimes non primes
          |${primes + nonPrimes} total
          |${sideLength(size)} side length
          |${primes / (primes + nonPrimes).toDouble} ratio""".stripMargin
    }

  }
}
