package hod.euler

import scala.collection.mutable

object Euler134 {
  def main(args: Array[String]): Unit = {
    val samples = {
      allPrimes
        .drop(2)
        .stopAfter(_ > 1000000)
    }
    val solutions = {
      samples
        .sliding(2, 1)
        .toVector
        .par
        .map { case Seq(p1, p2) =>
        val solution = {
          val endsWith = p1.toString
          val mod = math.pow(10, endsWith.length).toLong
          var test = p2.toLong
          while (test % mod != p1) {
            test += p2
          }
          test
        }
          print('.')
        //println(s"p1 = $p1, solution = $solution")
        solution
      }
    }
    val finalSolutions = solutions.sum
    println(finalSolutions)
  }
}
