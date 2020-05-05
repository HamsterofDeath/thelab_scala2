package hod.training

object Bools {
  def main(args: Array[String]): Unit = {
    def g(a:Boolean, b:Boolean, c:Boolean) = a^b^c

    val zero = false
    val one = true
    println(g(a = zero, b = zero , c = zero))
    println(g(a = zero, b = zero , c = one))
    println(g(a = zero, b = one , c = zero))
    println(g(a = zero, b = one , c = one))
    println(g(a = one, b = zero , c = zero))
    println(g(a = one, b = zero , c = one))
    println(g(a = one, b = one , c = zero))
    println(g(a = one, b = one , c = one))
  }
}
