package hod.trainingold.training

object FizzBuzz {
  def main(args: Array[String]): Unit = {
    val result = Array.tabulate[String](101)(_.toString)
    0.to(100, 3).foreach { where => result(where) = "Fizz" }
    0.to(100, 5).foreach { where => result(where) = "Buzz" }
    0.to(100, 15).foreach { where => result(where) = "FizzBuzz" }
    println(result.drop(1).mkString("\n"))
  }
}
