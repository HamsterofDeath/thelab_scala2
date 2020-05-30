package hod.training.scala

import hod.training.EmptyFrame

object ScalaSnake {
  class Spiel extends EmptyFrame {
    override def spielSchleife(): Unit = {

    }
  }

  def main(args: Array[String]): Unit = {
    new Spiel().start()
  }
}
