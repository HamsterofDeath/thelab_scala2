package hod.euler

import scala.collection.mutable

object Euler47 {
  def main(args: Array[String]): Unit = {
    val currentNumbers = mutable.ArrayBuffer.empty[Long]
    val searchFor = 4
    iterateLongs(1).foreach { i =>
      val factors = primeFactorsOf(i).toList
      if (factors.distinct.size != searchFor) {
        currentNumbers.clear()
      } else {
        currentNumbers += i
      }
      if (currentNumbers.size == searchFor) {
        println(currentNumbers)
        println(currentNumbers.head)
        System.exit(0)
      }
    }
  }
}
