package hod.trainingnew.rain

import hod.trainingold.training.EmptyFrame

import java.awt.Graphics

object Rain {
  def main(args: Array[String]): Unit = {
    val rs = new RainSimulator()
    rs.start()
  }

  class RainSimulator extends  EmptyFrame {
    override def zeichnen(g: Graphics): Unit = {
      super.zeichnen(g)
    }
  }
}
