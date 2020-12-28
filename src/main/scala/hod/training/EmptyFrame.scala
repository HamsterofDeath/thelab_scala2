package hod.training

import java.awt.event.{KeyAdapter, KeyEvent}
import java.awt.image.BufferedImage
import java.awt.{Color, Graphics}

import javax.swing.{JFrame, JPanel, Timer}


class EmptyFrame {
  protected def breite = 800
  protected def hoehe = 800

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
    val timer = new Timer(25, _ => spielSchleife())
    timer.setRepeats(true)
    timer.start()
  }

  protected var links,rechts,oben,unten,links2,rechts2,oben2,unten2 = false

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
          case KeyEvent.VK_W => oben2 = false
          case KeyEvent.VK_S => unten2 = false
          case KeyEvent.VK_A => links2 = false
          case KeyEvent.VK_D => rechts2 = false
          case _ =>
        }
      }

      override def keyPressed(e: KeyEvent): Unit = {
        e.getKeyCode match  {
          case KeyEvent.VK_UP => oben = true
          case KeyEvent.VK_DOWN => unten = true
          case KeyEvent.VK_LEFT => links = true
          case KeyEvent.VK_RIGHT => rechts = true
          case KeyEvent.VK_W => oben2 = true
          case KeyEvent.VK_S => unten2 = true
          case KeyEvent.VK_A => links2 = true
          case KeyEvent.VK_D => rechts2 = true
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
    links2 = false
    rechts2 = false
    oben2 = false
    unten2 = false
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
