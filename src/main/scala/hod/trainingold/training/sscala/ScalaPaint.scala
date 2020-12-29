package hod.trainingold.training.sscala

import java.awt.image.BufferedImage
import java.awt.{Color, Graphics}

import hod.trainingold.training.EmptyFrame

object ScalaPaint {
  var linksdingy = 500
  var rechtsdingy = 500
  var ballx = 500.0
  var bally = 500.0
  var ballGeschwindigkeitX = -5.0
  var ballGeschwindigkeitY = -5.0
  val schlaegerGeschwindigkeit =20


  def zufallXY = {
    val zufall = (math.random()*5-2.5).toInt
    if (zufall>0) zufall+4 else zufall-4
  }

  class Spiel extends EmptyFrame {

    override protected def breite: Int = 1000

    override protected def hoehe: Int = 1000

    lazy val bild = new BufferedImage(breite, hoehe, BufferedImage.TYPE_INT_ARGB)

    override def zeichnen(g: Graphics): Unit = {
      g.setColor(Color.WHITE)
      ballGeschwindigkeitX = ballGeschwindigkeitX*1.005
      ballGeschwindigkeitY = ballGeschwindigkeitY*1.005
      g.fillOval(ballx.toInt - 25, bally.toInt - 25, 50, 50)

      g.fillRect(0, linksdingy - 75, 50, 150)
      g.fillRect(950, rechtsdingy - 75, 50, 150)

      val schlaegerLinksObenY = linksdingy-75
      val schlaegerLinksUntenY = schlaegerLinksObenY+150
      val schlaegerLinksRechteSeiteX = 50

      if (ballx<schlaegerLinksRechteSeiteX &&
        bally>schlaegerLinksObenY &&
        bally<schlaegerLinksUntenY) {
        ballGeschwindigkeitX = -ballGeschwindigkeitX
      }

      val schlaegerRechtsObenY = rechtsdingy - 75
      val schlaegerRechtsUntenY = schlaegerRechtsObenY+150
      val schlaegerRechtsLinkeSeiteX = 950

      if (ballx>schlaegerRechtsLinkeSeiteX &&
        bally>schlaegerRechtsObenY &&
        bally<schlaegerRechtsUntenY) {
        ballGeschwindigkeitX = -ballGeschwindigkeitX
      }

      if (oben) {
        rechtsdingy = rechtsdingy - schlaegerGeschwindigkeit
      }

      if (unten) {
        rechtsdingy = rechtsdingy + schlaegerGeschwindigkeit
      }

      if (oben2) {
        linksdingy = linksdingy - schlaegerGeschwindigkeit
      }

      if (unten2) {
        linksdingy = linksdingy + schlaegerGeschwindigkeit
      }

      if (linksdingy < 0) {
        linksdingy = 0
      } else if (linksdingy > 1000) {
        linksdingy = 1000
      }

      if (rechtsdingy < 0) {
        rechtsdingy = 0
      } else if (rechtsdingy> 1000) {
        rechtsdingy = 1000
      }

      bally = bally + ballGeschwindigkeitY
      ballx = ballx + ballGeschwindigkeitX

      if (ballx< -50 || ballx > 1050) {
        ballx = 475
        bally = 475
        ballGeschwindigkeitX = zufallXY
        ballGeschwindigkeitY = zufallXY
      }

      if (bally<0 || bally > 1000) {
        ballGeschwindigkeitY = -ballGeschwindigkeitY
      }

    }
  }

  def main(args: Array[String]): Unit = {
    new Spiel().start()
  }
}
