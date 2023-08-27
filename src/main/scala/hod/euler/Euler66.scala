package hod.euler

object Euler66 {
  def main(args: Array[String]): Unit = {
    val solutions = {
      (1 to 1000)
        .filterNot(_.isPerfectSquare)
        .map { d =>
          val solution = {
            d.sqrtPrecise(75)
             .convergentFractions
             .find {
               case (x, y) =>
                 val result = x * x - d * y * y
                 result == 1
             }
             .openOr("math stopped working")
          }

          val (x, y) = solution
          (x, y, d)
        }
    }
    val finalSolution = solutions.maxBy(_._1)
    println(finalSolution)
  }
}
