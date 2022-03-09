package hod.other.trainingold.training

import scala.io.StdIn

object Öli {
  def main(args: Array[String]): Unit = {
    var text = "flasch"
    var x = 1

    def checkPasswort(pw: String, pw2: String):Unit = while (text != pw && text != pw2) {
      text = StdIn.readLine();if (x == 5) {println("Du bist TOT");return}

      if (text == pw || text == pw2) {println("hurrrrra")}
      else {x = x + 1;println("._.")}}

    checkPasswort("1234", "kindergarten")}}

