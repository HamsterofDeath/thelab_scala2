package hod.euler

import java.math.{BigDecimal, MathContext, RoundingMode}

object Euler80 {
  def main(args: Array[String]): Unit = {
    def digitSum(n: Int) = {
      val ctx = new MathContext(105, RoundingMode.HALF_UP)
      val rounded = new BigDecimal(n, ctx).sqrt(ctx)
      val str = rounded.toPlainString.filter(_ != '.').take(100)
      str.map(_.getNumericValue).sum
    }

    val solution = (1 to 100).filterNot {_.isPerfectSquare}.map(digitSum).sum
    println(solution)
  }
}
