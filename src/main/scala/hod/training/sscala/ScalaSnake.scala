package hod.training.sscala

import java.awt.{Color, Graphics}

import hod.training.EmptyFrame

object ScalaSnake {

  var spielerx = 0
  var spielery = 0

  var laufenx = 4
  var laufeny = 0

  val linksRechts = 60
  val obenUnten = 60
  val level = Array.fill(linksRechts, obenUnten)(0)
  class Spiel extends EmptyFrame {
    override def zeichnen(g: Graphics): Unit = {
      g.setColor(Color.GREEN)
      g.fillRoundRect(spielerx, spielery, 100, 100, 80, 80)

      if (oben) {
        laufeny = -4
        laufenx = 0
      } else if (unten) {
        laufeny = +4
        laufenx = 0
      } else if (links) {
        laufenx = -4
        laufeny = 0
      } else if (rechts) {
        laufenx = +4
        laufeny = 0
      }
      spielerx = spielerx + laufenx
      spielery = spielery + laufeny
    }
  }

  def main(args: Array[String]): Unit = {
    new Spiel().start()
  }
}
