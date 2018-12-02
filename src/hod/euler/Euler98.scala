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
    val anagrams = {
      words
      .groupBy(_.sorted)
      .values
      .toList
      .filter(_.length > 1)
    }
    val maxChars = {
      anagrams
      .map(_.head.length)
      .max
    }
    val maxValue = {
      math
      .pow(10, maxChars + 1)
      .toLong
    }
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
      def mappingsOf(word: String) = {
        val fittingSquares = squareNumbers.getOrElse(word.length, Nil)
        val possibleMappings = {
          fittingSquares.map { digits =>
            word.zip(digits)
          }.filter { digitToChar =>
            val sorted = digitToChar.sorted
            val digits = digitToChar.map(_._1)
            val chars = digitToChar.map(_._2)
            digits.allValuesDistinct &&
            chars.allValuesDistinct
          }.map(_.toMap)
        }
        possibleMappings
      }

      anagrams.flatMap { words =>
        val mappings = words.map(mappingsOf)
        val intersection = mappings.foldLeft(mappings.head)((acc, e) => acc.intersect(e))
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
    val withValues = {
      solutions.flatMap { case (groupOfAnagrams, mappings) =>
        groupOfAnagrams.map { word =>
          word -> mappings.map { mapping =>
            word.map(mapping)
          }
        }
      }
    }

    val largest = {
      withValues
      .maxBy { case (_, squares) =>
        squares.map(_.toLong).max
      }
    }
    println(largest)
  }
}
