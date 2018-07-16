package hod.euler

/**
  * Developed with pleasure
  * User: hamsterofdeath
  * Date: 07.12.12
  * Time: 23:09
  */
object Euler119 {
  def main(args: Array[String]) {
    def power(i: Int, power: Int) = BigInt(i).pow(power)

    val precalcPowers = {
      for (pow <- 2 to 10; sum <- 2 to 100) yield (sum, pow, power(sum, pow))
    }
    val correct = precalcPowers.filter({
      case (sum, pow, powered) =>
        val digits = powered.toString().map(_.getNumericValue).sum
        digits == sum && power(digits, pow) == powered
    }).sortBy(_._3)
    println(correct(29)._3)
  }
}