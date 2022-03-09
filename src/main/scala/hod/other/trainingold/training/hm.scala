package hod.other.trainingold.training

import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

object hm {
  def main(args: Array[String]): Unit = {

    //Die Wörter
    val Apfel = "Apfelbaum"

    val scanner = new Scanner(System.in)
    val schoneingegeben = ArrayBuffer.empty[Char]
    //Tot
    var Leben = 5
    def Anzeige = {
      Apfel.map { Buchstabe =>
        if (schoneingegeben.contains(Buchstabe.toLower)) {
          Buchstabe
        } else {
          '_'
        }
      }
    }

    while (Leben > 0 && Apfel!=Anzeige) {
      println(s"Du hast noch $Leben versuche")
      print("geb einen Buchstaben ein -> ")


      val eingabe = scanner.nextLine()
      println()

      val Richtich = Apfel.toLowerCase.contains(eingabe.toLowerCase)
      if (Richtich) {
        schoneingegeben ++= eingabe.map(_.toLower)
        println("ja")
      } else {
        println("nope")
        Leben -= 1
      }

      println(s"$eingabe <-- wurde eingegeben: $Anzeige")

    }
    if (Leben == 0)
      println("Leider hast du verloren")

  }
}
