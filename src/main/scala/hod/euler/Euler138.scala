package hod.euler

import scala.collection.parallel.CollectionConverters._

object Euler138 {
  def main(args: Array[String]): Unit = {
    var counter = 0L
    val solutions = Iterator
      .continually {
        counter += 1
        counter
      }.grouped(1000000)
      .flatMap { group =>
        val solutions = group.par.flatMap { halfB =>
          val twoHalves = halfB * 2
          Iterator(-1 + twoHalves, twoHalves, 1 + twoHalves).flatMap { h =>
            val bSqr = halfB * halfB
            val lSqr = bSqr + h * h
            if (lSqr.isPerfectSquare) {
              val solution = (halfB*2, h, lSqr.sqrtNatural)
              Some(solution)
            } else {
              None
            }
          }
        }.seq
        if (solutions.nonEmpty) {
          println(solutions.mkString("\n"))
        }
        solutions
      }
    println(solutions.take(12).toList)
  }
}
