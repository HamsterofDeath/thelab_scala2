package hod.euler

import scala.collection.parallel.CollectionConverters.RangeIsParallelizable

object Euler124 {
  def main(args: Array[String]): Unit = {
    val limit = 100000
    val check = 10000
    val tuples = (1 to limit).par.map {n =>
      val radical = primeFactorsOf(n).toSet.product
      (n, radical)
    }.toVector
    val sorted = tuples.sortBy(_._2)
    println(sorted(check-1)._1)
  }
}
