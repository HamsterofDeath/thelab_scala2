package hod.euler

object Euler132 {
  def main(args: Array[String]): Unit = {
    val k = 10.pow(9)
    val solution = allPrimes
      .filter { p =>
        BigInt(10).modPow(k, 9 * p) == 1
      }
      .take(40)
      .sum
    println(solution)
  }
}
