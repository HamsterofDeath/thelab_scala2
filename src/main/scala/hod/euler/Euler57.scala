package hod.euler

object Euler57 {
  def main(args: Array[String]): Unit = {
    trait Fraction {
      def toNormalFraction: NormalFraction

    }
    case class NormalFraction(numerator: BigInt, denominator: BigInt)
        extends Fraction {
      def plusOne: NormalFraction = {
        NormalFraction(numerator + denominator, denominator)
      }

      override def toNormalFraction: NormalFraction = this
    }
    case class ScaryFraction(
        numerator: BigInt,
        dNaturalPart: BigInt,
        dFractionPart: Fraction
    ) extends Fraction {
      override def toNormalFraction: NormalFraction = {
        val other = dFractionPart.toNormalFraction
        val subNumerator = dNaturalPart * other.denominator + other.numerator
        val subDenominator = other.denominator
        NormalFraction(subDenominator, subNumerator)
      }
    }
    def converge(steps: Int): Fraction = {
      steps match {
        case 1          => NormalFraction(1, 2)
        case n if n > 0 => ScaryFraction(1, 2, converge(steps - 1))
      }
    }

    val solution = 1 to 1000 map { steps =>
      converge(steps).toNormalFraction.plusOne
    } count { fraction =>
      fraction.numerator.getDigitCount > fraction.denominator.getDigitCount
    }
    println(solution)

  }
}
