package hod.trainingnew.rain

import hod.trainingold.training.EmptyFrame

import java.awt.{Color, Graphics}

object Rain {
  def main(args: Array[String]): Unit = {
    val rs = new RainSimulator()
    rs.start()
  }

  class RainSimulator extends EmptyFrame {
    override protected def breite: Int = 2000

    override protected def hoehe: Int = 2000

    var bewegungdeswürfelsy = (0)
    var bewegungdeswürfelsx = (0)

    override def zeichnen(g: Graphics): Unit = {
      super.zeichnen(g)
      g.setColor(Color.RED)
      bewegungdeswürfelsy = bewegungdeswürfelsy + 5
      bewegungdeswürfelsx = bewegungdeswürfelsx + 5
      g.drawRect(bewegungdeswürfelsx, bewegungdeswürfelsy, 50, 50)
    }

  }
}
