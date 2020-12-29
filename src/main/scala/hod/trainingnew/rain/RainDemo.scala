package hod.trainingnew.rain

import hod.trainingold.training.EmptyFrame

import java.awt.Graphics

object RainDemo {
  def main(args: Array[String]): Unit = {
    val rs = new RainSimulator()
    rs.start()
  }

  class RainSimulator extends EmptyFrame {

    override val breite = 2000

    var tropfen = List.empty[RegenTropfen]

    def zufallsTropfen = {
      val start = Position(zufall(breite),0)
      val bewegung = Bewegung(0.0, zufall(1.0))
      RegenTropfen(start, bewegung)
    }

    override def zeichnen(g: Graphics): Unit = {
      super.zeichnen(g)
    }

  }

  case class Position(woX: Double, woY: Double)
  case class Bewegung(x: Double, y: Double)
  case class RegenTropfen(position: Position, bewegung: Bewegung)

}
