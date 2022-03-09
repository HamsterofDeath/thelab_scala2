package hod.other.trainingold.training

object Random {
  def main(args: Array[String]): Unit = {
    List("10001111","11010101","00011110").foreach { e=>
      val num = Integer.parseInt(e,2)
      println(num.toBinaryString)
      println(num.toOctalString)
      println(num)
      println(num.toHexString)
    }
    16.toBinaryString
    16.toHexString
  }
}
