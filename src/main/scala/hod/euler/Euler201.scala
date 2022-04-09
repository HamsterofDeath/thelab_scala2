package hod.euler

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import javax.swing.SingleSelectionModel
import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.immutable.BitSet
import scala.collection.parallel.CollectionConverters.seqIsParallelizable
import scala.util.Random

object Euler201 {
  def main(args: Array[String]): Unit = {

    def testCase = Vector(1, 3, 6, 8, 10, 11)
    def realCase = {
      val maxNToSquare = 100
      (1 to maxNToSquare).map { n => n * n }
    }

    val numbers = realCase
    val indexOfNumber = {
      val data = numbers.zipWithIndex.toMap
      Array.tabulate(data.keys.max+1)(data.getOrElse(_,-1))
    }

    val setSize = numbers.size
    val combinationSize = setSize / 2
    val minSum = numbers.take(combinationSize).sum
    val maxSum = numbers.takeRight(combinationSize).sum
    val absoluteMin = 0
    val absoluteMax = numbers.sum

    println(minSum)
    println(maxSum)

    val bits = numbers.size * absoluteMax
    println(s"Required bits: $bits")
    println(s"Expects memory usage: ${bits / 8.0 / 1024 / 1024} mb")

    val lookUp = collection.mutable.BitSet
      .fromBitMaskNoCopy(Array.tabulate[Long](bits / 64 + 1)(_ => 0))
    def indexOf(sum: Int, using: Int) = {
      indexOfNumber(using) * absoluteMax + sum
    }

    def setTrue(sum: Int, using: Int) = {
      lookUp += indexOf(sum, using)
    }

    def isSummableUsing(sum: Int, using: Int): Boolean = {
      lookUp(indexOf(sum, using))
    }

    def hasAnyPath(sum: Int) = isSummableUsing(sum, numbers.last)

    def debug =
      numbers
        .map { n =>
          ((0 to absoluteMax)
            .map(isSummableUsing(_, n)))
            .map { if (_) "T" else "F" }
            .mkString
        }
        .mkString("\n")

    println("Building lookup table")
    val sumsToCheck = absoluteMin to maxSum
    numbers.zipWithIndex.foreach {
      case (using, index) =>
        print('.')
        sumsToCheck.foreach { sum =>
          val inherited = index > 0 && isSummableUsing(sum, numbers(index - 1))
          val fills =
            index > 0 && isSummableUsing(sum - using, numbers(index - 1))
          val canSum =
            sum == 0 ||
              sum == using ||
              inherited ||
              fills

          if (canSum) {
            setTrue(sum, using)
          }
        }
    }
    println("\nLookup done!")

    sealed trait PathType {
      def combine(other: PathType): PathType =
        (this, other) match {
          case (Single, Single) => Multi
          case (Multi, _)       => Multi
          case (_, Multi)       => Multi
          case (Single, _)      => Single
          case (_, Single)      => Single
          case _                => Undefined
        }

    }
    case object Undefined extends PathType
    case object Single extends PathType
    case object Multi extends PathType


    val hack = collection.mutable.HashMap.empty[(Int, Int, Int), PathType]
    def pathTypeToSum(
        sum: Int,
        using: Int,
        stepsLeft: Int,
        duplicateNotYetFound: Boolean
    ): PathType = {
      if (duplicateNotYetFound) {

      def evaluate = {
          if (using == numbers.head) {
            val endReached =
              (sum == using && stepsLeft == 1 || sum == 0 && stepsLeft == 0)
            if (endReached)
              Single
            else
              Undefined
          } else {
            val above = numbers(indexOfNumber(using) - 1)
            var paths: PathType = Undefined
            val inherited = isSummableUsing(sum, above)
            if (inherited) {
              paths = pathTypeToSum(sum, above, stepsLeft, true)
            }
            val fills = isSummableUsing(sum - using, above)
            if (fills) {
              val byFilling =
                pathTypeToSum(sum - using, above, stepsLeft - 1, paths != Multi)
              paths = paths.combine(byFilling)
            }
            paths
          }
        }
        hack.getOrElseUpdate((sum, using, stepsLeft), evaluate)
      } else {
        Multi
      }

    }

    def pathTypeOf(sum: Int) =
      if (hasAnyPath(sum)) pathTypeToSum(sum, numbers.last, combinationSize, true) else Undefined

    def backTrack(sum: Int, using: Int): List[List[Int]] = {
      if (isSummableUsing(sum, using)) {
        if (using == numbers.head) {
          if (sum == using) {
            List(List(using))
          } else if (sum == 0) {
            List(List())
          } else {
            throw new RuntimeException
          }
        } else {
          val previous = numbers(indexOfNumber(using) - 1)
          var paths = List.empty[List[Int]]
          val inherited = isSummableUsing(sum, previous)
          if (inherited) {
            val byInheritance = backTrack(sum, previous)
            paths ++= byInheritance
          }
          val fills = isSummableUsing(sum - using, previous)
          if (fills) {
            val byFilling =
              backTrack(sum - using, previous).map(using :: _)
            paths ++= byFilling
          }
          paths
        }
      } else {
        Nil
      }
    }

    def pathsOf(sum: Int) = {
      backTrack(sum, numbers.last)
    }

    val summables = measured {
      var counter = 0
      val ret = (minSum to maxSum).filter { sum =>
        counter += 1
        if (counter%1000==0) {
          print('.')
        }
        val uniquenessType = pathTypeOf(sum)
        val hit = uniquenessType == Single
        hit
      }
      println()
      ret
    }
    println()
    println(summables.sum)

//    println(debug)
  }
}
