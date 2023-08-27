package hod.euler

object Euler138 {
  def main(args: Array[String]): Unit = {
    //https://oeis.org/A007805
    def magic(n: Int): Long = {
      n match {
        case 0 => 1L
        case 1 => 17L
        case _ => 18L * magic(n - 1) - magic(n - 2)
      }

    }
    val solutions = 1 to 12 map magic
    println(solutions.sum)
  }
}
