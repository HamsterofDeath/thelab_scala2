package hod.euler

import scala.Option
import scala.collection.mutable
import scala.language.postfixOps

object Euler68 {
  def main(args: Array[String]): Unit = {
    case class DigitPlacement(digit: Int, where: Int)

    class NGonRing(n: Int, total:Int) {
      def isFull = selectedDigits.forall(_.isDefined)

      private val selectedDigits = Array.tabulate[Option[Int]](n * 2)(_ => None)
      private val freeDigits     = mutable.HashSet.empty[Int] ++= 1 to (n * 2)

      def solutionSetSimpleString = {
        solutionSets.map { triplet =>
          triplet.mkString("")
        }.mkString("")
      }

      def solutionSetsFormatted ={
        val details = {
          solutionSets.map { triplet =>
            triplet.mkString(", ")
          }.mkString("; ")
        }
        s"$total/${solutionSetSimpleString.size}: $details"
      }

      def solutionSets = {
        val sets = {
          0 until n map { i =>
            indexesOfTriplet(i).map(selectedDigits(_).get)
          }
        }
        val smallestIndex = {
          val min = sets.map(_.head).min
          sets.indexWhere(_.head == min)
        }

        val ret = {
          if (smallestIndex == 0) {
            sets
          } else {
            Iterator
            .continually(sets)
            .flatten
            .slice(smallestIndex, smallestIndex + n)
            .toList
          }
        }
        ret.distinct
      }

      def placements = {
        selectedDigits.zipWithIndex.collect { case (Some(digit), where) =>
          DigitPlacement(digit, where)
        }
      }

      def copy = {
        val ret = new NGonRing(n, total)
        placements.foreach(ret.placeDigit)
        ret
      }

      def placeDigit(digitPlacement: DigitPlacement): Unit = {
        assert(freeDigits.contains(digitPlacement.digit))
        assert(selectedDigits(digitPlacement.where).isEmpty)

        selectedDigits(digitPlacement.where) = Some(digitPlacement.digit)
        freeDigits -= digitPlacement.digit
      }

      def indexesOfTriplet(i: Int) = {
        if (i +1 < n) {
          Array(i + n, i, i + 1)
        } else {
          Array(i + n, i, 0)
        }
      }

      def placedAtIndex(i: Int) = {
        selectedDigits(i)
      }

      def nextFreeLine = {
        0 to n find { i =>
          val outer = indexesOfTriplet(i).head
          selectedDigits(outer).isEmpty
        }
      }

      def allSolutionsForNextLine = {
        nextFreeLine.map { index =>
          generateSolutionsFor(index).toList
        }.getOrElse(Nil)
      }

      def generateSolutionsFor(i: Int) = {
        val lineIndexes = indexesOfTriplet(i)
        val alreadyPlacedDigits = {
          lineIndexes.flatMap(placedAtIndex)
        }
        val alreadyPlacedWhere = {
          lineIndexes.filter(placedAtIndex(_).isDefined)
        }
        val alreadyPlacedSum = alreadyPlacedDigits.sum
        val freeIndexes = {
          lineIndexes.filterNot(alreadyPlacedWhere.contains)
        }

        freeDigits
        .toList
        .combinations(3 - alreadyPlacedDigits.length)
        .filter(_.sum + alreadyPlacedSum == total)
        .flatMap { digitsToPlace =>
          freeIndexes
          .permutations
          .map { putWhere =>
            digitsToPlace.zip(putWhere).map {
              case (a, b) => DigitPlacement(a, b)
            }
          }
        }
      }
    }
    def solve(operateOn: NGonRing) = {
      val solutions = mutable.ArrayBuffer.empty[NGonRing]

      def recur(operateOn: NGonRing): Unit = {
        if (operateOn.isFull) {
          solutions += operateOn
        } else {
          val solutionsForLine = operateOn.allSolutionsForNextLine
          solutionsForLine.foreach { solutionForLine =>
            val nextLevel = operateOn.copy
            solutionForLine.foreach(nextLevel.placeDigit)
            recur(nextLevel)
          }
        }
      }

      recur(operateOn)
      solutions
    }

    val solutions = {
      val minTotal = 1+2+3
      val maxTotal = 8+9+10
      val ngons = {
        (minTotal to maxTotal).par.flatMap { total =>
          solve(new NGonRing(5, total))
        }
      }
      ngons.seq.map(_.solutionSetSimpleString).distinct
    }

    val max = solutions.filter(_.length == 16).maxBy(_.toLong)
    println(max)
  }
}
