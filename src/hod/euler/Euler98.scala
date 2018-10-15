package hod.euler

import scala.reflect.io.File

object Euler98 {
  def main(args: Array[String]): Unit = {
    val file = File("resource/p98words.txt")
    val words = {
      file
      .slurp()
      .split(',')
      .map(_.drop(1).dropRight(1))
    }
    val anagrams = words.groupBy(_.sorted).values.toList.filter(_.length > 1)
    val maxChars = anagrams.map(_.head.length).max
    val maxValue = math.pow(10, maxChars + 1).toLong
    val squareNumbers = {
      allSquares
      .takeWhile(_ <= maxValue)
      .map(_.toString)
      .toList
      .groupBy(_.sorted)
      .values
      .toList
      .filter(_.length > 1)
      .flatten
      .groupBy(_.length)

    }
    val solutions = {
      anagrams.flatMap { words =>
        def mappingsOf(word: String) = {
          val fittingSquares = squareNumbers.getOrElse(word.length, Nil)
          val possibleMappings = {
            fittingSquares.map { digits =>
              digits.zip(word)
            }.filter { digitToChar =>
              val sorted = digitToChar.sorted
              sorted.toMap == sorted.reverse.toMap
            }.map(_.toMap)
          }
          possibleMappings
        }
        val mappings = words.map(mappingsOf)
        val intersection = mappings.foldLeft(mappings.head)((acc,e) => acc.intersect(e))
        val good = intersection.nonEmpty
        if (good) {
          Some {
            words.toList -> intersection
          }
        } else {
          None
        }
      }
    }
    solutions.map { case (words, mappings) =>

    }
  }
}
