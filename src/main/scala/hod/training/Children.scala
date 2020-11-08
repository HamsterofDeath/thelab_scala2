package hod.training

import scala.util.Random

object Children {
  def main(args: Array[String]): Unit = {
    val rnd = new Random()
    val boys = false
    def countBoysOfFamily  = Iterator.continually(rnd.nextBoolean()).takeWhile(_ == boys).size

    val families = 100000
    val totalBoys = (1 to families).map { _ =>
      val ret = countBoysOfFamily
      println(ret)
      ret
    }.sum
    val totalGirls = families

    println(s"$totalBoys vs $totalGirls")
  }
}
