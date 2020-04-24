package hod.euler

import java.io.File

object Euler89 {
  def main(args: Array[String]): Unit = {
    val numerals   = new File("resource/roman.txt").slurp.toList
    val totalChars = numerals.mkString.length

    def romanToDecimal(char: Char) = char match {
      case 'I' => 1
      case 'V' => 5
      case 'X' => 10
      case 'L' => 50
      case 'C' => 100
      case 'D' => 500
      case 'M' => 1000
    }

    val validRomans = List(
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
    )

    def highestRoman(value: Int) = {
      validRomans.find { suggestions =>
        romanStringToInteger(suggestions) <= value
      }.get
    }

    def romanStringToInteger(str: String) = {
      var current = Option.empty[Char]
      var next    = Option.empty[Char]

      def currentValue = romanToDecimal(current.get)

      def nextValue = romanToDecimal(next.get)

      var sum = 0
      str.foreach { char =>
        if (current.isEmpty) {
          current = Some(char)
        } else {
          next = Some(char)
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

    def integerToRoman(value: Int) = {
      var todo   = value
      var result = ""
      while (todo > 0) {
        val roman = highestRoman(todo)
        val next  = romanStringToInteger(roman)
        todo -= next
        result += roman
      }
      result
    }

    def optimize(roman: String) = integerToRoman(romanStringToInteger(roman))

    val totalCharsOptimized = numerals.map(optimize).map(_.length).sum

    println(s"saved: ${totalChars - totalCharsOptimized}")
  }
}
