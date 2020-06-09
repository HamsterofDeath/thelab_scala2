package hod.training.sscala

import java.awt.Graphics
import java.awt.image.BufferedImage

import hod.training.EmptyFrame

object ScalaPaint {

  class Spiel extends EmptyFrame {
    val bild = new BufferedImage(50,50, BufferedImage.TYPE_INT_ARGB)
    override def zeichnen(g: Graphics): Unit = {

    }
  }

  def main(args: Array[String]): Unit = {
    new Spiel().start()
  }
}
