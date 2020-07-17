package hod.training.sscala

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics}

import hod.training.EmptyFrame

object ScalaPaint {

  class Spiel extends EmptyFrame {
    val bild = new BufferedImage(500, 500, BufferedImage.TYPE_INT_ARGB)
    override def zeichnen(g: Graphics): Unit = {

      1 to 100 foreach { y =>
        1 to 100 foreach { x =>
          val farbe = new Color( y*2,17,x)
          val g2 = bild.getGraphics
          g2.setColor(farbe)
          val huh = 7
          g2.fillRect(x * huh, y * huh, huh, huh)
        }
      }

      g.drawImage(bild, 0, 0, null)
    }
  }

  def main(args: Array[String]): Unit = {
    new Spiel().start()
  }
}
