package hod.training

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.{Color, Graphics, Image}
import java.awt.image.BufferedImage

import javax.swing.{JFrame, JPanel, Timer}

object Labyrinth {
  private val breite = 800
  private val hoehe = 600

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


  private def zeichneLevel():Unit = {
    val g = workingImage.getGraphics
    g.setColor(Color.BLACK)
    g.fillRect(0, 0, breite, hoehe)
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
    hauptFenster.setSize(breite, hoehe)
  }

  def spielSchleife(): Unit = {
    bilder += 1
    zeichneLevel()
    finalImage.getGraphics.drawImage(workingImage, 0, 0, null)
    hauptFenster.repaint()
  }

  def main(args: Array[String]): Unit = {
    gefummel()
    starteTimer()
    hauptFenster.setVisible(true)
  }
}
