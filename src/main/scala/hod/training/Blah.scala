package hod.training

object Blah {
  def main(args: Array[String]): Unit = {
    0 to 9 foreach {x =>
      (0 to 10000).map(_.toString.toSeq.sorted.distinct) foreach { mat =>
        println(s"$x, $mat = ${mat.contains(x.toString.head)}")
      }
    }
  }
}
