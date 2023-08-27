package hod.euler

import java.io.File

import hod.EulerUtils

object Euler105 {
  def main(args: Array[String]): Unit = {
    measured {
      val sets = new File("resource/sets.txt").slurp.toList
                                              .map(_.split(',').map(_.trim.toInt).toList.sorted)
      var sum = 0
      sets.foreach { e =>
        if (EulerUtils.isSpecialSum(e)) {
          sum += e.sum
        }
      }
      println(s"Solution: $sum")
    }
  }
}
