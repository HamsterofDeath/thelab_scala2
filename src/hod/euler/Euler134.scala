package hod.euler

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object Euler134 {
  def main(args: Array[String]): Unit = {
    val samples = {
      allPrimes
        .drop(2)
        .stopAfter(_ > 1000000)
    }
    implicit val ctx = ExecutionContext.global
    def solutions = {
      samples
        .sliding(2, 1)
        .toVector
        .map { case Seq(p1, p2) =>
        Future {
          val solution = {
            val endsWith = p1.toString
            val mod      = math.pow(10, endsWith.length).toLong
            var test     = p2.toLong
            while (test % mod != p1) {
              test += p2
            }
            test
          }
          //println(s"p1 = $p1, solution = $solution")
          solution
        }
      }
    }
    def finalSolutions = solutions.map { e =>
      Await.result(e, Duration.Inf)
    }.sum
    measured {
      println(finalSolutions)
    }
  }
}
