package hod.training.sscala

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics}

import hod.training.EmptyFrame

object ScalaPaint {

  class Spiel extends EmptyFrame {
    val bild = new BufferedImage(50,50, BufferedImage.TYPE_INT_ARGB)
    override def zeichnen(g: Graphics): Unit = {
      1 to 10 foreach { x =>
        1 to 10 foreach { y =>
          bild.setRGB(10+x,10+y, new Color(0,0,255).getRGB)
        }
      }

      val g2 = bild.getGraphics
      g2.setColor(Color.green)
      g2.fillRect(5,5,5,5)
      g.drawImage(bild,25,25,50,50, null)


    }
  }

  def main(args: Array[String]): Unit = {
    new Spiel().start()
  }
}
