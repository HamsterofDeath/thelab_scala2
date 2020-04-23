package hod.euler

object Euler4 {
  def main(args: Array[String]): Unit = {
    val candidates = for {
      a <- 1 to 999
      b <- 1 to 999
    } yield {a * b}
    val solution   = candidates.filter(e => e.toString == e.toString.reverse).max
    println(solution)
  }
}
