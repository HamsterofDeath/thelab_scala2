package hod.trainingnew.wonderrocket

import hod.trainingnew.wonderrocket.WonderRocket.{blockSize, speed, triangleSize}
import hod.trainingold.training.EmptyFrame

import java.awt.{Color, Graphics, Polygon}
import scala.collection.mutable

object WonderRocket {
  val speed = 5
  val triangleSize = 20
  val blockSize = 20


  def main(args: Array[String]): Unit = {
    new Rocket().start()
  }
}

case class Enemy(x:Int, y:Int) {
  def down: Enemy = copy(y = y+3)

  val polygon = new Polygon(
    Array(x-triangleSize/2, x+triangleSize/2,x),
    Array(y-triangleSize/2, y-triangleSize/2,y),
    3
  )
}
class Rocket extends EmptyFrame {
  var rocketX = 0
  var ticks = 0

  var evil = List.empty[Enemy]
  var tot = false


  def drawRocket(g:Graphics) = {
    g.setColor(Color.WHITE)
    g.fillRect(rocketX, 700,blockSize,blockSize)

    if (!tot) {
      if ((links || links2) && rocketX > 0) {
        rocketX -= speed
      }
      if ((rechts || rechts2) && rocketX < breite - blockSize) {
        rocketX += speed
      }
    }
  }

  def drawDasBoese(g:Graphics) = {
    g.setColor(Color.RED)

    evil.foreach { feind =>
      g.fillPolygon(feind.polygon)
    }

    if (!tot) {
      evil = {
        evil
          .map(_.down)
          .filter(_.y < hoehe + triangleSize)
      }

      val addEnemy = {
        math.random()*5000 <= ticks
      }
      if (addEnemy) {
        evil = evil :+ Enemy(zufall(breite), -triangleSize)
      }

      tot |= evil.exists(_.polygon.intersects(rocketX, 700, blockSize, blockSize))
    }

  }

  override def zeichnen(g: Graphics): Unit = {
    g.setColor(Color.GREEN)
    if (tot) {
      g.drawString(s"Sie sind tot und haben nur $ticks Punkte",0,blockSize)
    } else {
      ticks += 1
      g.drawString(s"Score $ticks",0,blockSize)
    }
    drawRocket(g)
    drawDasBoese(g)
  }
}
