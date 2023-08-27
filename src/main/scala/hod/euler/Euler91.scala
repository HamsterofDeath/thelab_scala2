package hod.euler

import scala.collection.mutable

object Euler91 {

  def isRightAngled(x1: Int, y1: Int, x2: Int, y2: Int): Boolean = {
    val length1Squared = x1 * x1 + y1 * y1
    val length2Squared = x2 * x2 + y2 * y2
    val length3Squared = (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)
    val length1IsLarge =
      length1Squared > length2Squared && length1Squared > length3Squared
    if (length1IsLarge) {
      length1Squared == length2Squared + length3Squared
    } else {
      val length2IsLarge =
        length2Squared > length1Squared && length2Squared > length3Squared
      if (length2IsLarge) {
        length2Squared == length1Squared + length3Squared
      } else {
        length3Squared == length1Squared + length2Squared
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val size = 50
    var count = 0
    val range = 0 to size
    val seen = mutable.HashSet.empty[((Int, Int), (Int, Int))]
    range foreach { x1 =>
      range foreach { y1 =>
        range foreach { x2 =>
          range foreach { y2 =>
            val notInOrigin = !((x1 == 0 && y1 == 0) || (x2 == 0 && y2 == 0))
            val ordered     = x2 + y2 * (size + 1) > x1 + y1 * (size + 1)
            val notOnSamePoint = !(x1 == x2 && y1 == y2)
            if (
              notInOrigin &&
              ordered &&
              notOnSamePoint &&
              isRightAngled(x1, y1, x2, y2)
            ) {
              count += 1
            }
          }
        }
      }
    }
    println(s"Solution :$count")
  }
}
