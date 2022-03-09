package hod.other.trainingnew

import java.util.Scanner
import scala.util.Try

object Kenny {
  private def encode(text:String): String = text.map(encode).mkString

  private val chunkSize = 6

  private def encode(char: Char): String = {
    Integer.toUnsignedString(char.toInt, 3).map {
      case '0' => 'm'
      case '1' => 'p'
      case '2' => 'f'
    }.reverse.padTo(chunkSize,'m').reverse
  }

  private def decode(string:String) = {
    if (string.length%chunkSize==0) {
      string.grouped(chunkSize).map { encoded =>
        Integer.parseInt(encoded.map {
          case 'm' => '0'
          case 'p' => '1'
          case 'f' => '2'
        }, 3).toChar
      }.mkString
    } else {
      throw new RuntimeException
    }
  }

  private def transform(text:String) = {
    Try(decode(text)).getOrElse(encode(text))
  }

  def main(args: Array[String]): Unit = {
    val scanner = new Scanner(System.in)

    while (true) {
      println("Enter text:")
      val in = scanner.nextLine()
      println(transform(in))
    }
  }
}
