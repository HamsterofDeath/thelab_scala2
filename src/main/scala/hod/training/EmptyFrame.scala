package hod.training

import java.awt.event.{KeyAdapter, KeyEvent}
import java.awt.image.BufferedImage
import java.awt.{Color, Graphics}

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

  protected var links,rechts,oben,unten = false

  def gefummel(): Unit = {
    hauptFenster.setUndecorated(false)
    hauptFenster.add(grafikTeil)
    grafikTeil.setSize(breite, hoehe)
    hauptFenster.setSize(breite + 20, hoehe + 40)
    hauptFenster.addKeyListener(new KeyAdapter {

      override def keyReleased(e: KeyEvent): Unit = {
        e.getKeyCode match  {
          case KeyEvent.VK_UP => oben = false
          case KeyEvent.VK_DOWN => unten = false
          case KeyEvent.VK_LEFT => links = false
          case KeyEvent.VK_RIGHT => rechts = false
          case _ =>
        }
      }

      override def keyPressed(e: KeyEvent): Unit = {
        e.getKeyCode match  {
          case KeyEvent.VK_UP => oben = true
          case KeyEvent.VK_DOWN => unten = true
          case KeyEvent.VK_LEFT => links = true
          case KeyEvent.VK_RIGHT => rechts = true
          case _ =>
        }
      }
    })
  }

  private def resetControls = {
    links = false
    rechts = false
    oben = false
    unten = false
  }
  def zeichnen(g: Graphics) = {

  }

  def spielSchleife(): Unit = {
    val g = workingImage.getGraphics
    g.setColor(Color.black)
    g.fillRect(0,0, breite, hoehe)
    zeichnen(g)
    finalImage.getGraphics.drawImage(workingImage, 5, 5, null)
    hauptFenster.repaint()
  }

  def start(): Unit = {
    gefummel()
    starteTimer()
    hauptFenster.setVisible(true)
  }
}
