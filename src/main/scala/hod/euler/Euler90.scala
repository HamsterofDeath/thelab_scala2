package hod.euler

import scala.collection.mutable

object Euler90 {
  def main(args: Array[String]): Unit = {
    val desired = List("01", "04", "09", "16", "25", "36", "49", "64", "81")
    val pool = "0123456789".map(_.getNumericValue)
    val combinations = {
      pool
      .combinations(6)
      .map(_.toSet)
      .toList
    }

    val solutions = mutable.HashSet.empty[Set[Set[Int]]]

    combinations
    .foreach { die1 =>
      combinations
      .foreach { die2 =>
        val allSquaresCanBeFormed = {
          desired.forall { squareNumber =>
            val digit1 = squareNumber.head.getNumericValue
            val digit2 = squareNumber.last.getNumericValue

            def test(digit: Int, checkAgainst: Set[Int]) = {
              digit match {
                case 6 | 9 => checkAgainst(6) || checkAgainst(9)
                case n => checkAgainst(n)
              }
            }

            val canGiveNumber = {
              test(digit1, die1) && test(digit2, die2) ||
              test(digit1, die2) && test(digit2, die1)
            }
            canGiveNumber
          }
        }
        if (allSquaresCanBeFormed) {
          solutions += Set(die1, die2)
        }
      }
    }
    println(s"${solutions.size} solutions")
  }
}
