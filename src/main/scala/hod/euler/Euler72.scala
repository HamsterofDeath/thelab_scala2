package hod.euler

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.parallel.CollectionConverters._

object Euler72 {
  private def gcdEuclid2(a: Int, b: Int) = {
    var max = Math.max(a, b)
    var min = Math.min(a, b)
    var remainder = max % min
    while (remainder != 0) {
      max = min
      min = remainder
      remainder = max % min
    }
    min
  }

  def main(args: Array[String]): Unit = {
    measured {
      val max = 1000000
      val count = new AtomicInteger()
      val solution = (1 to max).par.map { n =>
        if (count.incrementAndGet() % 1000 == 0) {
          print(".")
        }
        (1 until n).count { d =>
          gcdEuclid2(d, n) == 1
        }
      }.map(_.toLong).sum
      println("\n" + solution)
    }
  }

}
