package hod.euler

object Euler3 {
  def main(args: Array[String]): Unit = {
    val num   = 600851475143L
    val limit = num.sqrt.toLong
    val solution = {
      allPrimesLong
        .takeWhile(_ <= limit)
        .filter { prime =>
          num % prime == 0
        }
        .max
    }
    println(solution)
  }
}
