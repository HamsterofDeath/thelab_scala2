package hod.euler

object Euler122 {
  def main(args: Array[String]): Unit = {
    def effExp(n:Int, path:List[Int], shortestSoFar:Int):Int = {
      if (shortestSoFar < path.length || n<0) {
        Int.MaxValue
      } else if (n == 0) {
        path.length - 1
      } else {
        val previous = path.head
        var newShortest = shortestSoFar
        path.iterator.distinct.map { subStep =>
          val newStepSize = previous + subStep
          val ret = effExp(n - subStep, newStepSize :: path, newShortest)
          newShortest = newShortest min ret
          ret
        }.min
      }
    }

    def solveFor(n:Int) = effExp(n-1, List(1), Int.MaxValue)

    val solution = (1 to  200).map { n =>
      val ret = solveFor(n)
      println(s"$n -> $ret")
      ret
    }.sum
    println(solution)
  }
}
