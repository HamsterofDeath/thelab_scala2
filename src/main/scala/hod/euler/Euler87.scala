package hod.euler

import scala.collection.mutable

object Euler87 {
  def main(args: Array[String]): Unit = {
    measured {
      val upperLimit = 50000000
      val maxA = upperLimit.sqrt.toLong
      val maxB = math.pow(upperLimit, 1.0 / 3).toLong
      val maxC = math.pow(upperLimit, 1.0 / 4).toLong

      var found = mutable.HashSet.empty[Int]
      val aPrimes = allPrimesLong.takeWhilePlusOne(_ <= maxA).toArray
      val bPrimes = aPrimes.iterator.takeWhilePlusOne(_ <= maxB).toArray
      val cPrimes = bPrimes.iterator.takeWhilePlusOne(_ <= maxC).toArray
      aPrimes.foreach { a =>
        val aSqr = a * a
        bPrimes.foreach { b =>
          val bCube = b * b * b
          val sumOfFirstTwo = (aSqr + bCube).toInt
          if (sumOfFirstTwo < upperLimit) {
            cPrimes.foreach { c =>
              val cQuad = c * c * c * c
              val num = sumOfFirstTwo + cQuad.toInt
              if (num < upperLimit) {
                found += num
              }
            }
          }
        }
      }
      println(s"Solution: ${found.size}")
    }
  }
}
