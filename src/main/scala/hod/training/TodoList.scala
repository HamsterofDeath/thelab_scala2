package hod.training

import java.util.Scanner

object TodoList {
  private var data = Array.fill[String](10)(null:String)
  def main(args: Array[String]): Unit = {
    val scanner = new Scanner(System.in)
    while(true) {
      println(
        s"""Welcome
           |1 = show
           |2 = add
           |3 = delete
           |4 = quit""".stripMargin)

      if (scanner.hasNextInt) {
        scanner.nextInt() match {
          case 1 =>
            println(s"In the list\n${data.takeWhile(_ != null).mkString("\n")}")
          case 2 =>
            val next = scanner.next()
            if (data.contains(next)) {
              println("nope")
            } else {
              data(data.indexOf(null)) = next
            }

          case 3 =>
            val remove = scanner.next()
            data = data.takeWhile(_ != remove) ++ data.reverse.takeWhile(_ != remove).reverse
          case 4 =>
            System.exit(0)
          case _ => println("i am confuse")
        }
      }
    }
  }
}
