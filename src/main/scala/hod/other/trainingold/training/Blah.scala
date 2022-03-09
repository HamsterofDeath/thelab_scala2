package hod.other.trainingold.training

import hod.euler.IntOps

object Blah {
  def main(args: Array[String]): Unit = {
    0 to 9 foreach {x =>
      (0 to 10000).map(_.toString.toSeq.sorted.distinct) foreach { mat =>
        println(s"$x, $mat = ${mat.contains(x.toString.head)}")
      }
    }
  }
}

object Wiggins {
  def main(args: Array[String]): Unit = {
    val start = 800
    Iterator.from(start).filter { test =>
      test.isPrime && (test-start).isPrime
    }.take(100).foreach(println)
  }
}
