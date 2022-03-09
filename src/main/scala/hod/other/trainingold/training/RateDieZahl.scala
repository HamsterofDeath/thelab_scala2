package hod.other.trainingold.training

import scala.util.{Failure, Success, Try}

object RateDieZahl {
  object Hilfe {
    def zufallsZahlBis100 = System.currentTimeMillis() % 100
    def leseZahl: Int = {
      Try {
        scala.io.StdIn.readLine().trim.toInt
      } match {
        case Failure(_) =>
          println("Das ist keine Zahl")
          leseZahl
        case Success(ret) => ret
      }

    }
  }

  def main(args: Array[String]): Unit = {
    val loesung = Hilfe.zufallsZahlBis100
    println("Rate die Zahl!")
    def eineRundeWeiter: Unit = {
      val versuch = Hilfe.leseZahl
      if (versuch > loesung) {
        println("Deine Zahl ist zu groß")
        eineRundeWeiter
      } else if (versuch < loesung) {
        println("Deine Zahl ist zu klein")
        eineRundeWeiter
      } else {
        println("Richtig!")
      }
    }

    eineRundeWeiter
    println("Das Spiel ist zuende, du hast gewonnen!")
  }
}
