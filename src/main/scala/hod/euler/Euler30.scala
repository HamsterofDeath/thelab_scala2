package hod.euler

object Euler30 {
  def main(args: Array[String]): Unit = {
    val solution = (1000 to 999999).filter { num =>
      val sum = num.allDigits.map(_.pow(5)).sum
      sum == num
    }.sum
    println(solution)
  }
}
