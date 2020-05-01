package hod.euler

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object Euler108 {

  implicit val ex = ExecutionContext.global
  def main(args: Array[String]): Unit = {
    def countSolutions(n: Long) = {
      var x = n * 2
      var solutions = 0
      do {
        val isCorrect = ((n * x) % (n - x)) == 0
        if (isCorrect) {
          solutions += 1
        }
        x -= 1
      } while (x > n)
      solutions
    }

    def parSearch: Int = {
      val best = new AtomicInteger
      val solution = Iterator.from(1).grouped(100).map { ns =>
        Await
          .result(
            Future.sequence(ns.map { n =>
              Future {
                val number = countSolutions(n)
                if (number > best.get()) {
                  best.set(number)
                  println(s"$number solutions for n=$n")
                }

                n -> number
              }
            }),
            Duration.Inf
          )
          .find(_._2 > 1000)
          .map(_._1) match {
          case Some(value) => return value
          case None        => // nop
        }
      }
      solution.foreach(_ => {})
      -1
    }
    measured {
      println(parSearch)
    }
  }
}


