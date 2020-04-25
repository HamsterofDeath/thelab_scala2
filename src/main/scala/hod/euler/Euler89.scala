package hod.euler

import java.io.File

object Euler89 {
  private def romanToDecimal(char: Char) = char match {
    case 'I' => 1
    case 'V' => 5
    case 'X' => 10
    case 'L' => 50
    case 'C' => 100
    case 'D' => 500
    case 'M' => 1000
  }

  private def highestRoman(value: Int) = {
    validRomans.find { suggestions =>
      suggestions._2 <= value
    }.get
  }

  private def romanStringToInteger(str: String) = {
    var current = ' '
    var next    = ' '

    def currentValue = romanToDecimal(current)

    def nextValue = romanToDecimal(next)

    var sum = 0
    str.foreach { char =>
      if (current == ' ') {
        current = char
      } else {
        next = char
        if (currentValue >= nextValue) {
          sum += currentValue
        } else {
          sum -= currentValue
        }
        current = next
      }
    }
    sum += currentValue
    sum
  }

  private val validRomans = List(
    "M",
    "CM",
    "D",
    "CD",
    "C",
    "XC",
    "L",
    "XL",
    "X",
    "IX",
    "V",
    "IV",
    "I"
  ).map { e =>
    (e, romanStringToInteger(e))
  }

  private def integerToRoman(value: Int) = {
    var todo   = value
    val result = new StringBuilder()
    while (todo > 0) {
      val (roman, singleValue) = highestRoman(todo)
      todo -= singleValue
      result append roman
    }
    result.toString
  }

  def main(args: Array[String]): Unit = {
    measured {
      solveIt
    }
  }

  private def solveIt = {
    val numerals   = new File("resource/roman.txt").slurp.toList
    val totalChars = numerals.mkString.length

    def optimize(roman: String) = integerToRoman(romanStringToInteger(roman))

    val totalCharsOptimized = numerals.map(optimize).map(_.length).sum

    println(s"saved: ${totalChars - totalCharsOptimized}")
  }
}
