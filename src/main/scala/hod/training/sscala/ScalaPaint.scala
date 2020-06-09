package hod.training.sscala

import java.awt.Graphics

import hod.training.EmptyFrame

object ScalaPaint {

  class Spiel extends EmptyFrame {
    override def zeichnen(g: Graphics): Unit = {
    }
  }

  def main(args: Array[String]): Unit = {
    new Spiel().start()
  }
}
