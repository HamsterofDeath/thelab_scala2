package hod.euler

import hod.EulerUtils

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.parallel.CollectionConverters._

object Euler72 {

  def main(args: Array[String]): Unit = {
    measured {
      val max = 1000000
      val count = new AtomicInteger()
      val solution = (1 to max).par.map { n =>
        if (count.incrementAndGet() % 1000 == 0) {
          print(".")
        }
        (1 until n).count { d =>
          gcdEuclid(d, n) == 1
        }
      }.map(_.toLong).sum
      println("\n" + solution)
    }
  }

}
