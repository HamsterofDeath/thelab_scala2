package hod.training.sscala

import java.awt.{Color, Graphics}

import hod.training.EmptyFrame

object ScalaSnake {

  var  spielerx= 0
var spielery= 0
  class Spiel extends EmptyFrame {
    override def zeichnen(g:Graphics): Unit = {
g.setColor(Color.GREEN)
  g.fillRoundRect(spielerx,spielery,100, 100,80,80)  }
    if (oben) spielery = spielery-1 else
    if (unten) spielery= spielery+1 else
    if (links) spielerx= spielerx-1 else
   if  ( rechts)spielerx = spielerx +1
  }

  def main(args: Array[String]): Unit = {
    new Spiel().start()
  }
}
