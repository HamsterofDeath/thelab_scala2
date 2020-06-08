package hod.euler

import scala.util.Try

object Euler93 {
  sealed trait Calculation {
    def calculate(a: BigDecimal, b: BigDecimal): BigDecimal
  }
  case object Add extends Calculation {
    override def calculate(a: BigDecimal, b: BigDecimal): BigDecimal = a + b
  }
  case object Sub extends Calculation {
    override def calculate(a: BigDecimal, b: BigDecimal): BigDecimal = a - b
  }
  case object SubFlip extends Calculation {
    override def calculate(a: BigDecimal, b: BigDecimal): BigDecimal = b - a
  }
  case object Mul extends Calculation {
    override def calculate(a: BigDecimal, b: BigDecimal): BigDecimal = a * b
  }
  case object DivFlip extends Calculation {
    override def calculate(a: BigDecimal, b: BigDecimal): BigDecimal = b / a
  }
  case object Div extends Calculation {
    override def calculate(a: BigDecimal, b: BigDecimal): BigDecimal = a / b
  }

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

  private def calculateConfiguration(digits: List[Int],
                                     calc: List[Calculation]) = {
    val concreteResults = {
      Try {
        val first = calc(0)
          .calculate(digits(0), digits(1))
        val second = calc(1)
          .calculate(first, digits(2))
        val third = calc(2)
          .calculate(second, digits(3))
        third
      }.toOption
    }
    concreteResults
      .filter(_.isWhole)
      .map(_.toInt)
      .filter(_ > 0)
  }
}
