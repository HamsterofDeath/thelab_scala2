package hod.euler

import java.io.File

object Euler79 {
  def main(args: Array[String]): Unit = {
    val in = ((new File("resource/keylog.txt").slurp.toList))
    def testNumber(str: String) = {
      in.forall { pin =>
        val indexes = pin.map(c => str.indexOf(c))
        indexes.forall(_ >= 0) &&
        indexes(0) < indexes(1) &&
        indexes(1) < indexes(2)
      }
    }

    def solutions =
      Iterator.from(3).flatMap { length =>
        val tens  = Iterator.continually(10)
        val start = tens.take(length - 1).product
        val end   = start * 10 - 1
        (start to end).iterator.map(_.toString).find(testNumber)
      }
    measured {
      println(solutions.next())
    }
  }
}
