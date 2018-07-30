package hod

import java.math.{BigInteger, MathContext, RoundingMode}
import scala.collection.mutable

package object euler {

  sealed trait ComparisonResult
  case object TargetIsSmaller extends ComparisonResult
  case object TargetIsEqual extends ComparisonResult
  case object TargetIsBigger extends ComparisonResult

  abstract class SearchSpace[T] {
    def nextHigherBound(t:T):T
    def nextLowerBound(t:T):T
    def determineMiddle(a:T, b:T):T
    def compareTargetAgainst(reference:T):ComparisonResult

  }

  def approachBinary[T](start:T, searchSpace:SearchSpace[T]): Iterator[T] = {
    var (min, max) = {
      var adjustableLimit = start
      def state: ComparisonResult = {
        searchSpace.compareTargetAgainst(adjustableLimit)
      }
      state match {
        case TargetIsSmaller =>
          while (state == TargetIsSmaller) {
            adjustableLimit = searchSpace.nextLowerBound(adjustableLimit)
          }
          (adjustableLimit, start)
        case TargetIsEqual =>
          (start, start)
        case TargetIsBigger =>
          while (state == TargetIsBigger) {
            adjustableLimit = searchSpace.nextHigherBound(adjustableLimit)
          }
          (start, adjustableLimit)
      }
    }

    var result = Option.empty[T]

    def nextCloserElement() = {
      val middle: T = searchSpace.determineMiddle(min, max)

      val cmpMin = searchSpace.compareTargetAgainst(min)
      val cmpMiddle = searchSpace.compareTargetAgainst(middle)
      val cmpMax = searchSpace.compareTargetAgainst(max)
      val nextTry = {
        (cmpMin, cmpMiddle, cmpMax) match {
          case (TargetIsBigger, TargetIsSmaller,_) =>
            max = middle
          case (_, TargetIsBigger,TargetIsSmaller) =>
            min = middle
          case (TargetIsEqual,_,_) =>
            result = Some(min)
          case (_,TargetIsEqual,_) =>
            result = Some(middle)
          case (_,_,TargetIsEqual) =>
            result = Some(max)
          case trip@_ => throw new RuntimeException(s"inconsistent state: $trip on $min, $middle, $max")
        }
        middle
      }
      nextTry
    }
    Iterator.continually(nextCloserElement()).stopAfter(_ => result.nonEmpty)

  }

  def allPrimes: Iterator[Int] = {
    Iterator(2, 3) ++ Iterator.from(5, 2).filter(_.isPrime)
  }

  def primes(n: Int): Iterator[Int] = {
    allPrimes.take(n)
  }

  def allSquares: Iterator[BigInt] = {
    Iterator
    .from(1)
    .map { e =>
      BigInt(e) * e
    }
  }

  implicit class IntOps(val i: Int) extends AnyVal {

    def isPrime: Boolean = {
      if (i % 2 == 0) return i == 2
      if (i % 3 == 0) return i == 3
      val sqrtn = Math.sqrt(i)
      var j = 5
      var step = 4
      while (j <= sqrtn) {
        if (i % j == 0) return false
        step = 6 - step
        j += step
      }
      true
    }
  }

  implicit class LongOps(val l: Long) extends AnyVal {

    def sqrtPrecise(scale: Int): BigDecimal = {
      val mc = new MathContext(scale, RoundingMode.HALF_UP)
      BigDecimal(java.math.BigDecimal.valueOf(l).sqrt(mc), mc)
    }

    def isPerfectSquare: Boolean = {
      val n = l
      if (n < 0) {
        false
      } else {
        (n & 0x3F).toInt match {
          case 0x00 | 0x01 | 0x04 | 0x09 | 0x10 | 0x11 | 0x19 | 0x21 | 0x24 | 0x29 | 0x31 | 0x39 =>
            var sqrt = 0L
            if (n < 410881L) { //John Carmack hack, converted to Java.
            // See: http://www.codemaestro.com/reviews/9
            var i = 0
              var x2 = .0F
              var y = .0F
              x2 = n * 0.5F
              y = n
              i = java.lang.Float.floatToRawIntBits(y)
              i = 0x5f3759df - (i >> 1)
              y = java.lang.Float.intBitsToFloat(i)
              y = y * (1.5F - (x2 * y * y))
              sqrt = (1.0F / y).toLong
            }
            else { //Carmack hack gives incorrect answer for n >= 410881.
              sqrt = Math.sqrt(n).toLong
            }
            sqrt * sqrt == n
          case _ =>
            false
        }
      }
    }

    def isPerfectSquareSlow: Boolean = {
      l != 0 && {
        val ref = java.math.BigInteger.valueOf(l)
        val sqrt = ref.sqrt
        sqrt.pow(2) == ref
      }
    }

    def isPrime: Boolean = {
      if (l % 2 == 0) return l == 2
      if (l % 3 == 0) return l == 3
      val sqrtN = Math.sqrt(l)
      var j = 5
      var step = 4
      while (j <= sqrtN) {
        if (l % j == 0) return false
        step = 6 - step
        j += step
      }
      true
    }
  }

  implicit class IteratorOps[T](val it: Iterator[T]) extends AnyVal {
    def memoizedByIndex: Int => T = {
      val cache = mutable.HashMap.empty[Int, T]
      var maxEvaluated = -1
      i: Int => {
        assert(i >= 0, "int overflow")
        while (i > maxEvaluated) {
          val t = it.next()
          maxEvaluated += 1
          cache.put(maxEvaluated, t)
          maxEvaluated
        }
        cache(i)
      }
    }

    def stopAfter(isLastAccepted:T => Boolean): Iterator[T] = {
      var stop = false
      it.takeWhile { e =>
        val take = !stop
        stop = isLastAccepted(e)
        take
      }
    }
  }

  def stop() = {
    println("stop")
  }

  implicit class BigDecimalOps(val bd: BigDecimal) extends AnyVal {
    def continuedFractions: Iterator[Long] = {
      var remaining = bd

      def nextWhole: Long = remaining.toLong

      Iterator.continually {
        val nextValue = nextWhole
        remaining = {
          val num = BigDecimal(1, bd.mc)
          val denom = remaining - BigDecimal(nextValue, bd.mc)
          num / denom
        }
        nextValue
      }
    }

    def convergentFractions: Iterator[(BigInt, BigInt)] = {
      val fractions = continuedFractions.memoizedByIndex
      val forDenominator = (i: Int) => fractions(i + 1)
      val cache = mutable.HashMap.empty[(Int, Boolean), BigInt]

      def eval(n: Int, isNumerator: Boolean): BigInt = {
        cache.getOrElseUpdate((n, isNumerator), {
          val on = if (isNumerator) fractions else forDenominator
          n match {
            case larger if larger > 2 =>
              val a = on(larger - 2)
              val b = eval(larger - 1, isNumerator)
              val c = eval(larger - 2, isNumerator)
              a * b + c
            case 2 => on(0)
            case 1 => BigInt(1)
            case _ => throw new IllegalArgumentException(n.toString)
          }
        })
      }

      Iterator.from(1).map { n =>
        val num = eval(n + 1, isNumerator = true)
        val denom = eval(n, isNumerator = false)
        (num, denom)
      }
    }

  }

  implicit class BigIntegerOps(val bi: BigInteger) extends AnyVal {

    def isPerfectSquare: Boolean = {
      val root = bi.sqrt
      (root multiply root) == bi
    }
  }

  implicit class DoubleOps(val d: Double) extends AnyVal {

    def isPerfectSquare: Boolean = {
      d != 0 &&
      math.sqrt(d).isNatural
    }

    def sqrt: Double = math.sqrt(d)

    def sqrtPrecise(scale: Int): BigDecimal = {
      java.math.BigDecimal.valueOf(d)
      .sqrt(new MathContext(scale, RoundingMode.HALF_UP))
    }

    def isNatural: Boolean = {
      d.floor == d &&
      !d.isNaN &&
      !d.isInfinity
    }

  }

  implicit class OptionOps[T](val o: Option[T]) extends AnyVal {
    def openOrEval(excuse: => String): T = {
      o match {
        case Some(x) => x
        case None => throw new RuntimeException(excuse)
      }
    }

    def openOr(excuse: String): T = {
      o match {
        case Some(x) => x
        case None => throw new RuntimeException(excuse)
      }
    }
  }

}
