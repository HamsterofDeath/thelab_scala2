package hod.other.trainingold.training

object Logic {
  def main(args: Array[String]): Unit = {
    val options = Integer.parseInt("000000", 2) to Integer.parseInt("111111", 2)
    val solutions = options.flatMap { int =>
      val string = int.toBinaryString.reverse.padTo(6, '0')
      val flags = string.reverse.map { char =>
        char=='1'
      }

      val Seq(a,b,c,d,e,f) = flags.toList

      val aOK = !d || !c || !b
      val bOk = if (d) c else false
      val cOk = if (e && a) f else false
      val dOk = if (b && e) a else false
      val eOk = if (!b) f else false
      val fOk = if (e) b else false

      val ret =
          aOK==a &&
          bOk==b &&
          cOk==c &&
          dOk==d &&
          eOk==e &&
          fOk==f
      println(s"$string:$flags")
      if(ret) Some(string) else None

    }
    println(solutions.mkString("\n"))
  }
}
