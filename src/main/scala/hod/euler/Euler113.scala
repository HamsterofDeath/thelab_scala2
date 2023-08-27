package hod.euler

object Euler113 {
  def main(args: Array[String]): Unit = {
    val hack = collection.mutable.HashMap.empty[(Int, Boolean, Int, Int), Long]

    def recur(
               current: Int,
               asc: Boolean,
               usedDigitCount: Int,
               totalDigits: Int
             ): Long = {
      def eval = {
        val exhausted =
          current == 10 || current == -1 || usedDigitCount == totalDigits
        val invalid   = exhausted && (usedDigitCount < totalDigits)
        if (invalid)
          0
        else if (exhausted) {
          1
        } else {
          val maxTake = totalDigits - usedDigitCount
          val minTake = {
            if (current == 9 && asc || current == 0 && !asc) maxTake else 0
          }
          (for (digitCountToAdd <- minTake to maxTake) yield {
            recur(
              current + (if (asc) 1 else -1),
              asc,
              usedDigitCount + digitCountToAdd,
              totalDigits
            )
          }).sum
        }
      }
      hack.getOrElseUpdate((current, asc, usedDigitCount, totalDigits), eval)
    }

    var sumSoFar = 9L // to cover 1 digit

    def countUntilNDigits(n: Int) = {
      println(s"Solution for 1 digits: 9")
      val subSums = ((2 to n)).map { digits =>
        val nonBouncyAsc = recur(1, true, 0, digits)
        val nonBouncyDesc = recur(9, false, 0, digits)
        val solution = nonBouncyAsc + nonBouncyDesc - 10
        sumSoFar += solution
        println(s"Solution for $digits digits: $solution, total: $sumSoFar")
        solution
      }
      9 + subSums.sum
    }

    println(s"Non bouncy total :" + countUntilNDigits(100))
  }
}
