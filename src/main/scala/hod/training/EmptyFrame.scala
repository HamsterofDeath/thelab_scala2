package hod.training

import java.awt.{Color, Graphics}
import java.awt.image.BufferedImage

import javax.swing.{JFrame, JPanel, Timer}

class EmptyFrame {
  private val breite = 800
  private val hoehe = 800

  val workingImage =
    new BufferedImage(breite, hoehe, BufferedImage.TYPE_INT_ARGB)

  val finalImage = new BufferedImage(breite, hoehe, BufferedImage.TYPE_INT_ARGB)

  val hauptFenster = new JFrame

  val grafikTeil:JPanel = new JPanel() {
    override def paintComponent(g: Graphics): Unit = {
      g.drawImage(finalImage, 0, 0, null)
    }
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
  }

  def spielSchleife(): Unit = {

    val g = workingImage.getGraphics
    g.setColor(Color.black)
    g.fillRect(0,0, breite, hoehe)
    finalImage.getGraphics.drawImage(workingImage, 5, 5, null)
    hauptFenster.repaint()
  }

  def start(): Unit = {
    gefummel()
    starteTimer()
    hauptFenster.setVisible(true)
  }
}
