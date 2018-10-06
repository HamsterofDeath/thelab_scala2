package hod.euler

object Euler100 {
  def main(args: Array[String]): Unit = {
    def t(b:Long) ={
      val magic = {
        BigInt(8L) * b * b - BigInt(8) * b + 1
      }
      val magic2 = magic.sqrt(25)
      0.5d * (magic2 + 1)
    }

    val hard = 100000000000L
    val easy = 1L
    Iterator.iterate(easy)(_ + 1).foreach { b =>
      val total = t(b)
      if (total.isValidLong) {
        println(s"$b blue, ${total - b} red")
      }
    }
  }
}
