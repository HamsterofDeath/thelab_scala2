package hod.euler

import java.io.File

object Euler99 {
  def main(args: Array[String]): Unit = {
    trait HasDoubleValue {def toDouble:Double}
    case class ExponentialD(base:Int, power:Double) extends HasDoubleValue {
      override def toDouble: Double = Math.pow(base,power)
    }
    case class Exponential(base:Int, power:Int) extends HasDoubleValue {
      override def toDouble: Double = Math.pow(base,power)
      def deExponentified(reduceBy: Int):HasDoubleValue = {
        if (reduceBy==power) Exponential(base,1) else ExponentialD(base, power/reduceBy.toDouble)
      }

    }

    def returnSmaller(a:Exponential,b:Exponential) = {
      val smallerExponent = a.power min b.power
      val reducedA = a.deExponentified(smallerExponent)
      val reducedB = b.deExponentified(smallerExponent)
      if (reducedA.toDouble<reducedB.toDouble) a else b
    }

    val in = new File("resource/p99.txt").slurp.toList.map { str =>
      val Array(base, power) = str.split(',')
      Exponential(base.toInt, power.toInt)
    }

    val largest = in.max(Ordering.fromLessThan[Exponential]((a,b) => returnSmaller(a,b)==a))
    println(in.indexOf(largest)+1)
  }

}
