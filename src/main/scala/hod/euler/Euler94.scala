package hod.euler

import java.util.concurrent.atomic.AtomicLong
import scala.collection.parallel.CollectionConverters._

object Euler94 {
  def main(args: Array[String]): Unit = {

    val solutions = new AtomicLong(0)

    Iterator
      .from(2)
      .flatMap(side => List((side, side-1), (side, side+1)))
      .filter(_._2 % 2 == 0)
      .takeWhile {
        case (sides, bottom) => sides + sides + bottom <= 1000000000
      }
      .grouped(2000000)
      .foreach { group =>
        println(s"Reached ${group.head._1.nice}")
        val subSolutions = group.par.filter {
          case (sides, bottom) =>
            val cSqr = sides.toLong * sides
            val aSqr = (bottom / 2).pow(2)
            val bSqr = cSqr - aSqr
            bSqr.isPerfectSquareSlow
        }
        val part = subSolutions.map(e => e._1.toLong + e._1 + e._2).sum
        solutions.addAndGet(part)
      }

    println(solutions)
  }
}
