package hod.other.trainingnew

import java.util.Scanner

object tictictic {
  def main(args: Array[String]): Unit = {

    val felder = Array.fill(10)(" ")


    def schreibeFeld = {
      s"""
     ||-----------|
     || ${felder(7)} | ${felder(8)} | ${felder(9)} |
     ||---|---|---|
     || ${felder(4)} | ${felder(5)} | ${felder(6)} |
     ||---|---|---|
     || ${felder(1)} | ${felder(2)} | ${felder(3)} |
     ||-----------|
     |""".stripMargin
    }

    val scanner = new Scanner(System.in)

    var zeichen = "X"

    def hatGewonnen(wer:String) = {
      felder(1) == wer && felder(5)== wer && felder(9)== wer||
      felder(3) == wer && felder(5)== wer && felder(7)== wer||
      felder(1) == wer && felder(4)== wer && felder(7)== wer||
      felder(2) == wer && felder(5)== wer && felder(8)== wer||
      felder(3) == wer && felder(6)== wer && felder(9)== wer||
      felder(1) == wer && felder(2)== wer && felder(3)== wer||
      felder(4) == wer && felder(5)== wer && felder(6)== wer||
      felder(7) == wer && felder(8)== wer && felder(9)== wer
    }

    def istVoll = {
        (felder(1)=="X"||felder(1)=="O") &&
        (felder(2)=="X"||felder(2)=="O") &&
        (felder(3)=="X"||felder(3)=="O") &&
        (felder(4)=="X"||felder(4)=="O") &&
        (felder(5)=="X"||felder(5)=="O") &&
        (felder(6)=="X"||felder(6)=="O") &&
        (felder(7)=="X"||felder(7)=="O") &&
        (felder(8)=="X"||felder(8)=="O") &&
        (felder(9)=="X"||felder(9)=="O")
    }












    def spielZuEnde = hatGewonnen("X")||hatGewonnen("O")||istVoll
    def nochEineRunde = !spielZuEnde
    while (nochEineRunde) {
      println("wohin?")
      val eingabe = scanner.nextInt()
      val schonBesetzt = (felder(eingabe)=="X"||felder(eingabe)=="O")

      if (schonBesetzt) {
        println("nope")
      } else {
        felder(eingabe) = zeichen

        if (zeichen == "X") {
          zeichen = "O"
        } else {
          zeichen = "X"
        }

        if (hatGewonnen("X")) {
          println("X hat gewonnen!")
        }

        if (hatGewonnen("O")) {
          println("O hat gewonnen!")
        }
        print(schreibeFeld)

      }




  }

    print("Zeit für nervige aufgaben")

}}
