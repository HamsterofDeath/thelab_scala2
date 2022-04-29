package hod.euler

object Euler254_todo {
  def main(args: Array[String]): Unit = {

    def orderedLongs[X](cb: BigInt => X) = {
      val maxDigits = 30

      def recur(remainingDigits: Int, minValue: Int, value: BigInt): Unit = {
        if (remainingDigits == 0) {
          cb(value)
        } else {
          minValue to 9 foreach { nextDigit =>
            recur(remainingDigits - 1, nextDigit, value * 10 + nextDigit)
          }
        }
      }

      0 to maxDigits foreach { length =>
        recur(length, 1, 0)
      }
    }

    def factorial(n: Int): Int = {
      singleDigitFactorial(n)
    }

    def f(n: BigInt) =
      n.digits.map(factorial).sum

    def sf(n: BigInt) = f(n).allDigits.sum

    val limit = 150
    val lowest = collection.mutable.HashMap.empty[Int, BigInt]
    def v2 = {
      orderedLongs { digits =>
        val value = sf(digits)
        if (value <= limit) {
          lowest.get(value) match {
            case Some(stored) if stored > digits =>
              lowest.put(value, digits)
              println(value + "-->" + f(digits) +" > " +digits + " (update)")
            case None =>
              lowest.put(value, digits)
              println(value + "-->" + f(digits) +" > " +digits)
            case _ =>
          }
        }
      }
    }

    v2
    println(lowest.toList.sorted.mkString("\n"))
  }
}
