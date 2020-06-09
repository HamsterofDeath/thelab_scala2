package hod.training.sscala

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics}

import hod.training.EmptyFrame

object ScalaPaint {

  class Spiel extends EmptyFrame {
    val bild = new BufferedImage(500,500, BufferedImage.TYPE_INT_ARGB)
    override def zeichnen(g: Graphics): Unit = {

     var xxx = 1

      1 to 100 foreach { y =>
      xxx=xxx+1
        1 to 100  foreach { x =>
        val farbe = if (x%2==0) Color.blue else Color.yellow
          bild.setRGB(10+x*2,10+y*2, farbe.getRGB)
          bild.setRGB(10+x*2+1,10+y*2, farbe.getRGB)
          bild.setRGB(10+x*2,10+y*2+1, farbe.getRGB)
          bild.setRGB(10+x*2+1,10+y*2+1, farbe.getRGB)
        }
      }


      g.drawImage(bild,25,25,null)


    }
  }

  def main(args: Array[String]): Unit = {
    new Spiel().start()
  }
}
