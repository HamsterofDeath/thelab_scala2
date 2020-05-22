package hod.euler

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Euler265 {

  def validRing(ring: ArrayBuffer[Int],n:Int): Boolean = {
    val subSequences = (0 to ring.size).map { start =>
      (0 until n).map { r =>
        ring((start+r)%ring.size)
      } .mkString
    }
    subSequences.distinct.size == ring.size
  }

  def main(args: Array[String]): Unit = {
    def rings(n:Int):List[List[Int]] = {
      val targetSize = 2.pow(n)
      val solutions = mutable.ArrayBuffer.empty[List[Int]]
      val ring = ArrayBuffer.fill[Int](n)(0)
      val seen = mutable.HashSet.empty[List[Int]]
      seen += List.fill[Int](n)(0)
      val tries = List(0,1)
      def recur():Unit = {
        if (ring.size < targetSize) {
          tries.foreach { next =>
            ring += next
            val subSeq = ring.takeRight(n).toList
            if (!seen(subSeq)) {
              seen += subSeq
              recur()
              seen -= subSeq
            }
            ring.dropRightInPlace(1)
          }
        }  else {
          if (validRing(ring,n)) {
            solutions += ring.toList
          }
        }
      }
      recur()
      solutions.toList
    }
    val solution = rings(5).map(_.mkString).map(java.lang.Long.parseLong(_, 2)).sum
    println(solution)
  }
}
