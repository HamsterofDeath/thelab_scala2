package hod.euler

object Euler43 {
  def main(args: Array[String]): Unit = {
    val divisibility = Array(2,3,5,7,11,13,17)
    val solution = {
      "0123456789"
        .toSeq
        .permutations
        .map(_.unwrap)
        .filter { str =>
          str
            .drop(1)
            .toSeq
            .sliding(3,1)
            .zipWithIndex
            .forall { case (subString, index) =>
              subString.unwrap.toInt % divisibility(index) == 0
            }
        }.map(_.toLong)
        .sum
    }
    println(solution)
  }
}
