package hod.training.jjava;

import hod.training.EmptyFrame;

import java.awt.Color;
import java.awt.Graphics;

public class JavaSnake {
    public static void main(String[] args) {
      new Spiel().start();
    }
}

class Spiel extends EmptyFrame {
    @Override
    public void zeichnen(final Graphics g) {
        g.setColor(Color.CYAN);
        g.drawOval(50,50,50,50);
    }
}
