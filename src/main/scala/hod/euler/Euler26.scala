package hod.euler

import scala.collection.mutable

object Euler26 {
  def main(args: Array[String]): Unit = {
    def postZeroDigits(n: Int) = {
      var left = 1
      val right = n
      val digits = mutable.ArrayBuffer.empty[Int]
      val knownRemainders = mutable.HashSet.empty[Int]
      val remaindersInOrder = mutable.ArrayBuffer.empty[Int]
      def done = left == 0 || knownRemainders(left)
      while (!done) {
        knownRemainders += left
        remaindersInOrder += left
        while (left < right) {
          left *= 10
        }
        digits += left / right
        left = left % right
      }
      if (knownRemainders(left)) {
        val recurringCycleMarker = left
        val from = {
          remaindersInOrder.indexOf(recurringCycleMarker)
        }

        (digits.take(from), digits.slice(from, digits.size))
      } else {
        (digits, Nil)
      }
    }
    1 to 999 foreach { i =>
      val (digits, cycle) = postZeroDigits(i)
      println(s"$i -> 0.${digits.mkString}(${cycle.mkString})")
    }
  }
}
