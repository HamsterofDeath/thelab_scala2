package hod.euler

import java.io.File
import scala.collection.mutable

import hod.EulerUtils

object Euler103 {
  def main(args: Array[String]): Unit = {

    def findLowestSumFor(n: Int, assumedMax: Int, limitSum:Option[Int] = None): Array[Int] = {
      var best = limitSum
      val workingSet = (1 to n).toArray
      val workingSetMax = {
        workingSet.zipWithIndex
          .map {
            case (_, i) =>
              assumedMax - (n - i - 1)
          }
      }
      def next(): Unit = {
        def incByOne(where: Int): Unit = {
          if (workingSet(where) < workingSetMax(where)) {
            workingSet(where) += 1
          } else {
            if (where>0) {
              incByOne(where - 1)
              workingSet(where) = workingSet(where - 1) + 1
            }
          }
        }
        incByOne(n - 1)
      }
      while (true) {
        val couldBeCandidate = best.exists(_ > workingSet.sum) || best.isEmpty
        if (couldBeCandidate) {
          val inCandidate = EulerUtils.isSpecialSum(workingSet)
          if (inCandidate) {
            best = best match {
              case Some(value) => Some(value min workingSet.sum)
              case None        => Some(workingSet.sum)
            }
            println(s"$best -> ${workingSet.mkString(", ")}")
            return workingSet // first hit happens to be the solution
          }
        }

        next()
      }
      workingSet
    }

    measured {
      val ints = findLowestSumFor(7, 45, Some(256))
      println(s"solution: ${ints.mkString(", ")} = ${ints.sum}")
    }
  }
}
