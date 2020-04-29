package hod.euler

import java.io.File
import scala.collection.mutable

object Euler105 {
  def main(args: Array[String]): Unit = {
    def isSpecialSum(set:Iterable[Int]): Boolean = {
      val list = set.toList.sorted
      // check size rule
      (2 until list.length).foreach { n =>
        val maxWithN = list.takeRight(n).sum
        val minWithNPlusOne = list.take(n+1).sum
        if (minWithNPlusOne <= maxWithN) {
          return false
        }
      }
      // check sum rule
      (2 until list.length).foreach { n =>
        val seenSums = mutable.HashSet.empty[Int]
        list.combinations(n).foreach { array =>
          val sum = array.sum
          if (seenSums(sum)) {
            return false
          }
          seenSums += sum
        }
      }

      true
    }

    measured {
      val sets   = new File("resource/sets.txt").slurp.toList.map(_.split(',').map(_.trim.toInt).toList.sorted)
      var sum = 0
      sets.foreach { e =>
        if (isSpecialSum(e)) {
          sum += e.sum
        }
      }
      println(s"Solution: $sum")
    }
  }
}
