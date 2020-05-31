package hod.euler

object Euler112 {
  def main(args: Array[String]): Unit = {
    def isBouncy(num: Long): Boolean = {
      var lastDigit = -1
      var inc = false
      var dec = false

      num.allDigitsReversed.foreach { digit =>
        if (lastDigit == -1) {
          // nop
        } else {
          inc |= lastDigit > digit
          dec |= lastDigit < digit
        }
        if (inc && dec) return true
        lastDigit = digit
      }
      false
    }

    var current = 0L
    var bouncies = 0L
    def proportionMatches = {
      if (current > 0) {
        BigDecimal(bouncies) / current == BigDecimal("0.99")
      } else {
        false
      }
    }
    measured {
      while (!proportionMatches) {
        current += 1
        if (isBouncy(current)) {
          bouncies += 1
        }

      }
      println(s"$bouncies of $current,prop: ${BigDecimal(bouncies) / current}")
    }
  }
}
