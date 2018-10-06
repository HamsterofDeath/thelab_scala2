package hod.euler

object Euler100 {
  def main(args: Array[String]): Unit = {
    def evalBOfTotal(t:Long) = {
      // thx to wolfram alpha
      val tt2 = BigInt(2L) * t * t
      val t2p1 = BigInt(2L) * t
      ((tt2 - t2p1 + 1).sqrt(25)+1) * 0.5d
    }
    def evalTotalOf(b:Long) ={
      val magic = {
        BigInt(8L) * b * b - BigInt(8) * b + 1
      }
      val magic2 = magic.sqrt(25)
      0.5d * (magic2 + 1)
    }

    val hard = 10000000000000L
    val easy = 21L
    Iterator.iterate(hard)(_ + 1).foreach { t =>
      val blues = evalBOfTotal(t)
      if (blues.isValidLong) {
        val red = t - blues
        println(s"$blues blue, ${red} red = ${blues+red} total, result = ${(blues/t) * ((blues-1)/(t-1))}")
      }
    }
  }
}
