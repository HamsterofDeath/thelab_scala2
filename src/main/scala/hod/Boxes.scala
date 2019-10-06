package hod

object Boxes {
  def main(args: Array[String]): Unit = {
    val setups = {
      (2 to 30).map { firstBox =>
        (firstBox, firstBox + 6, 30 - 6 - firstBox * 2)
      }.filter {
        case (a, b, c) => a + b + c == 30 && c > 0
      }.map {
        case (a, b, c) => List(a, b, c)
      }.flatMap(_.permutations)
    }
    val guesses = {
      1 to 30 flatMap { a =>
        1 to 30 flatMap { b =>
          1 to 30 map { c =>
            List(a, b, c)
          }
        }
      }
    }
    val results = {
      guesses.map { guess =>
        def profit(inBox: Int, guess: Int) = {
          if (guess <= inBox) guess else 0
        }

        val won = {
          val sums = {
            setups.map { boxes =>
              guess.zip(boxes).map { case (guessForBox, gems) => profit(gems, guessForBox) }.sum
            }
          }
          guess -> sums//sums.sum.toDouble / sums.size
        }
        won
      }.distinct
    }
    val readMe = results.sortBy(e => e._2.sum.toDouble / e._2.size).reverse
    val stats = readMe.map { case (a,b) => a -> b.groupBy(identity).map { case (a, c) => c.size -> a}}
    println(stats)
  }
}
