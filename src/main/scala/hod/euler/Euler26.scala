package hod.euler

import scala.collection.mutable

object Euler26 {
  def main(args: Array[String]): Unit = {
    measured {
      def postZeroDigits(n: Int) = {
        var left = 1
        val right = n
        val digits = mutable.ArrayBuffer.empty[Int]
        val remaindersInOrder = mutable.ArrayBuffer.empty[Int]
        def done = left == 0 || remaindersInOrder.contains(left)
        while (!done) {
          remaindersInOrder += left
          while (left < right) {
            left *= 10
          }
          digits += left / right
          left = left % right
        }
        if (remaindersInOrder.contains(left)) {
          val recurringCycleMarker = left
          val from = {
            remaindersInOrder.indexOf(recurringCycleMarker)
          }

          (digits.take(from), digits.slice(from, digits.size))
        } else {
          (digits, Nil)
        }
      }
      val solution = (1 to 999).iterator.maxBy { i =>
        postZeroDigits(i)._2.size
      }
      println(solution)
    }
  }
}
