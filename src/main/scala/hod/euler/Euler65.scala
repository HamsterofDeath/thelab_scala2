package hod.euler

import java.math.MathContext

object Euler65 {


  def main(args: Array[String]): Unit = {
    val e = {
      euler(5000,1000)
    }
    e.convergentFractions.slice(99, 100).foreach {
      case (n,d) => println(n.toString().map(Character.getNumericValue).sum)
    }
  }
}
