package hod.other.trainingold.training

import java.awt.event.{KeyAdapter, KeyEvent}
import java.awt.{Color, Graphics}
import java.awt.image.BufferedImage
import scala.collection.mutable
import scala.util.Random

import javax.swing.{JFrame, JPanel, Timer}

object LabyrinthGenerator {
  def neuesLabyrinth(dimX: Int, dimY: Int, fillPercent: Int) = {
    val data = Array.fill(dimX, dimY)(" ")
    val rnd = new Random
    var remaining = (dimX * dimY * (fillPercent / 100.0)).toInt
    val randomXY = Iterator.continually {
      (rnd.nextInt(dimX) -> rnd.nextInt(dimY))
    }
    def isValid(targetX: Int, targetY: Int) = {
      val checked = mutable.BitSet.empty
      def flood(x: Int, y: Int): Boolean = {
        if (x < 0 || y < 0 || x >= dimX || y >= dimY) {
          false
        } else if (x == targetX && y == targetY) {
          checked += x + y * dimX
          true
        } else if (data(x)(y) == " " && !checked(x + y * dimX)) {
          checked += x + y * dimX
          val ret = flood(x - 1, y) |
            flood(x + 1, y) |
            flood(x, y + 1) |
            flood(x, y - 1)
          ret
        } else {
          false
        }
      }
      (flood(0, 0), checked.size)
    }

    var addOneMore = true
    var canTakeBack = 1000
    var freeFields = dimX * dimY
    while (addOneMore && remaining > 0 && canTakeBack > 0) {

      val (fillX, fillY) = randomXY.next()
      if (data(fillX)(fillY) == " ") {
        data(fillX)(fillY) = "X"

        val (valid, reachableFields) = isValid(dimX - 1, dimY - 1)
        addOneMore = valid

        if (addOneMore && reachableFields == freeFields - 1) {
          remaining -= 1
          freeFields -= 1
        } else {
          data(fillX)(fillY) = " "
          canTakeBack -= 1
          addOneMore = true
        }
      }
    }

    data.toList.map(_.mkString)
  }
}

object Labyrinth {
  private val breite = 800
  private val hoehe = 800

  val workingImage =
    new BufferedImage(breite, hoehe, BufferedImage.TYPE_INT_ARGB)

  val finalImage = new BufferedImage(breite, hoehe, BufferedImage.TYPE_INT_ARGB)

  val hauptFenster = new JFrame

  val grafikTeil = new JPanel() {
    override def paintComponent(g: Graphics): Unit = {
      g.drawImage(finalImage, 0, 0, null)
    }
  }

  var bilder = 0

  val karte = LabyrinthGenerator.neuesLabyrinth(25, 25, 55)

  val linksRechtsAnzahl = karte(0).size
  val obenUntenAnzahl = karte.size
  val blockBreite = breite / linksRechtsAnzahl
  val blockHoehe = hoehe / obenUntenAnzahl

  private def zeichneAlleBloecke(g: Graphics): Unit = {

    g.setColor(Color.WHITE)

    0 to (linksRechtsAnzahl - 1) foreach { x =>
      0 to (obenUntenAnzahl - 1) foreach { y =>
        val levelStueck = karte(y)(x)
        if (levelStueck == 'X')
          g.fillRect(
            blockBreite * x,
            y * blockHoehe,
            blockBreite - 1,
            blockHoehe - 1
          )

      }

    }
    spielerSteuerung
    zeicheSpieler(spx, spy, g)

  }

  private def spielerSteuerung = {
    if (nachLinks) {
      spx = spx - 1
    } else if (nachOben) {
      spy = spy - 1
    } else if (nachRechts) {
      spx = spx + 1
    } else if (nachUnten) {
      spy = spy + 1
    } else {
      // nix
    }
    nachOben = false
    nachLinks = false
    nachRechts = false
    nachUnten = false
  }

  def zeicheSpieler(x: Int, y: Int, g: Graphics) = {
    val woSpielerX = x * blockBreite
    val woSpielerY = y * blockHoehe
    g.setColor(Color.blue)
    g.fillRect(woSpielerX, woSpielerY, blockBreite, blockHoehe)
  }

  var spx = 0
  var spy = 1

  //steuerung
  var nachOben = false
  var nachUnten = false
  var nachLinks = false
  var nachRechts = false

  private def zeichneLevel(): Unit = {
    val g = workingImage.getGraphics
    g.setColor(Color.BLACK)
    g.fillRect(0, 0, breite, hoehe)

    zeichneAlleBloecke(g)

    g.setColor(Color.WHITE)
    g.drawString(bilder + " mal geloopt", 0, 15)
  }

  def starteTimer(): Unit = {
    val timer = new Timer(100, _ => spielSchleife())
    timer.setRepeats(true)
    timer.start()
  }

  def gefummel(): Unit = {
    hauptFenster.setUndecorated(false)
    hauptFenster.add(grafikTeil)
    grafikTeil.setSize(breite, hoehe)
    hauptFenster.setSize(breite + 20, hoehe + 40)
    hauptFenster.addKeyListener(new KeyAdapter {
      override def keyReleased(e: KeyEvent): Unit = {
        if (e.getKeyCode == KeyEvent.VK_UP) {
          nachOben = true
        } else if (e.getKeyCode == KeyEvent.VK_DOWN) {
          nachUnten = true
        } else if (e.getKeyCode == KeyEvent.VK_LEFT) {
          nachLinks = true
        } else if (e.getKeyCode == KeyEvent.VK_RIGHT) {
          nachRechts = true
        }
      }
    })
  }

  def spielSchleife(): Unit = {
    bilder += 1
    zeichneLevel()
    finalImage.getGraphics.drawImage(workingImage, 5, 5, null)
    hauptFenster.repaint()
  }

  def main(args: Array[String]): Unit = {
    gefummel()
    starteTimer()
    hauptFenster.setVisible(true)
  }
}
