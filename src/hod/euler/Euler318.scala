package hod.euler

import java.math.{MathContext, RoundingMode}
import java.text.DecimalFormat
import scala.collection.mutable

object Euler318 {
  def main(args: Array[String]): Unit = {
    val nines = 2011

    val pairsToTest = {
      val unfiltered = {
        1 to nines flatMap { p =>
          1 to nines map { q =>
            (p, q)
          }
        }
      }
      unfiltered.filter {
        case (p, q) => p < q && p + q <= nines && p > 1
      }.sortBy { case (p, q) => p + q }
    //        List((2,3))
    }

    type NumberType = Int

    def fractionNineCount(n: NumberType, base: BigDecimal): Int = {
      val onlyFrac = base
      //val powered = onlyFrac.pow(BigDecimal(n * 2))
      val powered = {
        val requiredPrecision = {
          50000
        }
        BigDecimal(onlyFrac.bigDecimal, new MathContext(requiredPrecision, RoundingMode.HALF_UP)).pow(n * 2)
      }
      val fractional = powered
      val asString = {
        val fraction = fractional.fractionalPart
        fraction.bigDecimal.toPlainString
      }
      val ninesFound = asString.iterator.drop(2).takeWhile(_ == '9').size
      println(s"$n -> $ninesFound (total size ${asString.length - 2})")
      ninesFound
    }

    pairsToTest.foreach { case (p, q) =>
      val cache = mutable.HashMap.empty[NumberType, Int]

      val scale = 50000
      val pSqrt = p.sqrtPrecise(scale)
      val qSqrt = q.sqrtPrecise(scale)
      val sum = {
        val mySum = pSqrt.bigDecimal.add(qSqrt.bigDecimal)
        BigDecimal(mySum)
      }

      def fractionCached(n:NumberType, base:BigDecimal) = {
        cache.getOrElseUpdate(n, fractionNineCount(n, base))
      }
      println(s"solving for $p / $q")
      val solution = {
        val start = nines + nines / 2
        approachBinary[NumberType](start, new SearchSpace[NumberType] {
          override def nextHigherBound(t: NumberType): NumberType = t * 2
          override def nextLowerBound(t: NumberType): NumberType = t / 2
          override def determineMiddle(a: NumberType, b: NumberType): NumberType = (a + b) / 2
          override def compareTargetAgainst(reference: NumberType): ComparisonResult = {
            val oneLess = reference - 1
            val lowerNines = fractionCached(oneLess, sum)
            val currentSampleNines = fractionCached(reference, sum)
            (lowerNines, currentSampleNines) match {
              case (l, h) if l < nines && h >= nines => TargetIsEqual
              case (_, h) if h >= nines => TargetIsSmaller
              case (_, h) if h < nines => TargetIsBigger
              case a@_ => throw new RuntimeException(s"unexpected state: $a")
            }
          }
        }).lastElement
      }
      val finalResult = sum.pow(solution * 2)
      println(s"(sqrt($p)+sqrt($q))^${2 * solution} = a lot")
    }
  }
}
