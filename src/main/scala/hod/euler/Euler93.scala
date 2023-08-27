package hod.euler

import scala.util.Try

object Euler93 {
  type Calculation = (Double, Double) => Double
  val Add    : Calculation = (a, b) => a + b
  val Mul    : Calculation = (a, b) => a * b
  val Div    : Calculation = (a, b) => a / b
  val Sub    : Calculation = (a, b) => a - b
  val DivFlip: Calculation = (a, b) => b / a
  val SubFlip: Calculation = (a, b) => b - a

  def main(args: Array[String]): Unit = {
    val solution = measured {
      val calculations =
        List(Add, Sub, Mul, Div, DivFlip, SubFlip).flatMap(e => List(e, e, e))
      val comb =
        calculations.combinations(3).toList.flatMap(_.permutations).distinct

      def findAll(digits: List[Int]) = {
        val reachable = digits.permutations
          .flatMap { digitPerm =>
            comb.flatMap { calc =>
              calculateConfiguration(digitPerm, calc)
            }
          }
          .toSet
          .toList
          .sorted
        reachable
      }

      val pool = (1 to 9).combinations(4).map(_.toList).toList
      pool
        .map { subSet =>
          val chainUntil = {
            val reachable = findAll(subSet)

            reachable
              .sliding(2, 1)
              .takeWhile {
                case List(a, b) =>
                  a + 1 == b
              }
              .size + 1
          }
          (subSet, chainUntil)
        }
        .maxBy(_._2)
    }
    println(s"Solution: $solution")
  }

  private def calculateConfiguration(
                                      digits: List[Int],
                                      calc: List[Calculation]
                                    ) = {
    val concreteResults = {
      Try {
        val first = calc(0)(digits(0), digits(1))
        val second = calc(1)(first, digits(2))
        val third = calc(2)(second, digits(3))
        third
      }.toOption
    }
    concreteResults
      .filter(_.isWhole)
      .map(_.toInt)
      .filter(_ > 0)
  }
}
