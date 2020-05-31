package hod.training.sscala

import java.awt.{Color, Graphics}

import hod.training.EmptyFrame

object ScalaSnake {
  class Spiel extends EmptyFrame {
    override def zeichnen(g:Graphics): Unit = {
      g.setColor(Color.green)
      g.fillRoundRect(0,0,100,100,75,75)
    }
  }

  def main(args: Array[String]): Unit = {
    new Spiel().start()
  }
}
