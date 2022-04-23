package hod.euler

import scala.collection.Searching

object Euler123 {
  def limit = 10.pow(10)


  def binary = {
    val src = allPrimesLong
      .zipWithIndex
      .map { case (a,b) => (a,b+1)}
      .filter(_._2%2==1)
      .take(100000)
      .toVector

    val tooLazyToThink = src.map(_.swap).toMap

    def index2prime(i:Int) = tooLazyToThink(i)

    def toCheckValue(index:Int) = {
      val p = index2prime(index)
      val a = (p - 1).powSafe(index)
      val b = (p + 1).powSafe(index)
      val modBy = p.sqr
      val check = (a + b) % modBy
      println(s"($p, $index) => (($p-1)^$index + ($p+1)^$index) % $modBy = $check")
      check.bigInteger.longValueExact()
    }

    val indexes = src.map(_._2)
    val sr = indexes.view.map(toCheckValue).search(limit)
    sr match {
      case Searching.Found(foundIndex) => println(src((foundIndex)))
      case Searching.InsertionPoint(insertionPoint) => println(src((insertionPoint)))
    }

  }

  def linear = {
    val firstPN = allPrimesLong
      .zipWithIndex
      .map { case (a,b) => (a,b+1)}
      .find { case (p,index) =>
        index % 2 ==1 && {
          val a = (p - 1).powSafe(index)
          val b = (p + 1).powSafe(index)
          val modBy = p.sqr
          val check = (a + b) % modBy
          println(s"($p, $index) => (($p-1)^$index + ($p+1)^$index) % $modBy = $check")
          check >limit
        }
      }
    println(firstPN)
  }

  def main(args: Array[String]): Unit = {
    binary

  }

}
