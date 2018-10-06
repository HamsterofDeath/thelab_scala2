package hod.euler

object Euler100 {
  def main(args: Array[String]): Unit = {
    // wolfram alpha gave me the formulas :D

    val requiredPrecision = 25

    def evalROfB(b: Long) = {
      // 1/2 (sqrt(8 b^2 - 8 b + 1) - 2 b + 1)
      val sqrtMe = {
        val left = BigInt(8) * b * b
        val right = BigInt(8) * b
        left - right + 1
      }
      val sub = BigInt(2) * b
      (sqrtMe.sqrt(requiredPrecision) - sub.toBigDecimal + 1) * 0.5d
    }

    def evalBOfTotal(t: Long) = {
      val left = BigInt(2L) * t * t
      val right = BigInt(2L) * t
      ((left - right + 1).sqrt(requiredPrecision) + 1) * 0.5d
    }

    def evalBOfR(r: Long) = {
      // 1/2 (sqrt(8 r^2 + 1) + 2 r + 1)
      val sqrtMe = {
        BigInt(8) * r * r + 1
      }
      val add = BigInt(2) * r + 1
      (sqrtMe.sqrt(requiredPrecision) + add.toBigDecimal) * 0.5d
    }

    def evalTotalOfB(b: Long) = {
      //1/2 (sqrt(8 b^2 - 8 b + 1) + 1
      val sqrtMe = {
        val left = BigInt(8L) * b * b
        val sub = BigInt(8L) * b
        left - sub + 1
      }
      0.5d * (sqrtMe.sqrt(requiredPrecision) + 1)
    }

    val hard = 10000000000000L

    val minB = evalBOfTotal(hard).toLong
    val minR = 5 //evalROfB(minB).toLong

    val validBlues = {
      List(1, 3, 15,
        85, 493, 2871,
        16731, 97513, 568345,
        3312555, 19306983, 112529341,
        655869061, 3822685023L, 22280241075L,
        129858761425L, 756872327473L, 4411375203411L,
        25711378892991L, 149856898154533L, 873430010034205L)
    }

    val firstSolution = validBlues.find(b => evalTotalOfB(b) > 1000000000000L)
    println(firstSolution)

/*
    var tries = 0L
    Iterator.iterate(minR)(_ + 1).foreach { red =>
      tries += 1
      if (tries % 100000 == 0) {
        print('.')
      }
      val blues = evalBOfR(red)
      if (blues.isValidLong) {
        val blueLong = blues.toLongExact
        val total = blueLong + red
        val prob = (blueLong.toDouble / total) * ((blueLong.toDouble - 1) / (total - 1))
        println(s"\n$blueLong blue, $red red = ${blues + red} total, result = ${prob}")
      }
    }
*/
  }
}
