package hod.euler

object Euler2 {
  def main(args: Array[String]): Unit = {
    var start = (1, 2)

    def next: Int = {
      start = (start._2, start._1 + start._2)
      start._2
    }

    val fibs     = Iterator.single(1) ++ Iterator.single(2) ++ Iterator.continually(next)
    val solution = fibs.filter(_ % 2 == 0).takeWhile(_ <= 4000000).sum
    println(solution)
  }
}

object Euler2CStyle {
  def main(args: Array[String]): Unit = {
    var (first, second, sum) = (1, 2, -12345)
    sum = first + second
    var sumOfAllEven = 2
    while (sum <= 4000000) {
      if (sum % 2 == 0) {
        sumOfAllEven += sum
      }
      first = second
      second = sum
      sum = first + second
    }
    println(sumOfAllEven)

  }
}

