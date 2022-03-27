package hod.euler

object Euler164 {
  def main(args: Array[String]): Unit = {
    val maxDigits = 20

    val hack = collection.mutable.HashMap.empty[(String, Int), BigInt]
    def recur(current:BigInt, index:Int):BigInt = {
      val endReached = index == maxDigits
      if (endReached) {
        1
      } else {
        val previousTwoDigits = (current % 100).toString()
        val sumOfPreviousDigits = {
          previousTwoDigits.map(Character.getNumericValue).sum
        }
        def eval:BigInt = {
          val maxNextDigit = 9 - sumOfPreviousDigits

          val minNextDigit = if (index == 0) 1 else 0
          val subSums = for {
            nextDigit <- minNextDigit to maxNextDigit
          } yield recur(current*10+nextDigit, index + 1)
          subSums.sum
        }
        val key = previousTwoDigits
        hack.getOrElseUpdate((key, index), eval)
      }
    }

    val solution = recur(0, 0)
    println("smart: "+solution)
  }
}
