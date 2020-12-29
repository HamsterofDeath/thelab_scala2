package hod.trainingold.training.sscala

import java.awt.{Color, Graphics}

import hod.trainingold.training.EmptyFrame

object ScalaSnake {

  var spielerx = 0
  var spielery = 0

  var laufenx = 10
  var laufeny = 0

  var lange = 5

  val linksRechts = 800
  val obenUnten = 800
  val level = Array.fill(linksRechts, obenUnten)(0)
  class Spiel extends EmptyFrame {
    override def zeichnen(g: Graphics): Unit = {
      g.setColor(Color.GREEN)
      g.fillRoundRect(spielerx, spielery, 10, 10, 80, 80)

      if (oben) {
        laufeny = -10
        laufenx = 0
      } else if (unten) {
        laufeny = +10
        laufenx = 0
      } else if (links) {
        laufenx = -10
        laufeny = 0
      } else if (rechts) {
        laufenx = +10
        laufeny = 0
      }
      spielerx = spielerx + laufenx
      spielery = spielery + laufeny


      0 to (level.length - 1) foreach { x =>
        0 to (level(x).length - 1) foreach { y =>

          level(x)(y) = level(x)(y) - 1
          if (level(x)(y) > 0) {
            g.drawString(level(x)(y).toString ,x, y)
            //g.fillRoundRect(x, y, 10, 10, 80, 80)
            if (spielerx == x & spielery == y) {
              System.exit(0)
            }
          }

        }
      }

      level(spielerx)(spielery) = lange

    }
  }

  def main(args: Array[String]): Unit = {
    new Spiel().start()
  }
}
