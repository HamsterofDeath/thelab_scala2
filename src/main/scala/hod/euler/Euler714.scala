package hod.euler

import scala.collection.parallel.CollectionConverters._

object Euler714 {
  val replacements = {
    (0 to 9 flatMap { first =>
      0 to 9 map { second =>
        List(first, second)
      }
    }).distinct
      .map { e => (e.head.toString.head, e.last.toString.head) }
  }

  def allDuoDigits = {

    Iterator.from(1).flatMap { digits =>
      val min = Integer.parseInt("1".padTo(digits, '0'), 2)
      val max = Integer.parseInt("1".padTo(digits, '1'), 2)
      (min to max).flatMap { dec =>
        val binaryString = dec.toBinaryString
        forBinaryString(binaryString)
      }.sorted.iterator
    }
  }

  def forBinaryString(binaryString:String) = {
    replacements.map { case (zero, one) =>
      binaryString.map {
        case x if x == '0' => zero
        case x if x == '1' => one
      }
    }.filter(_.head != '0')
      .map { e =>
        BigInt(e)
      }
      .distinct
      .sorted
  }

  def main(args: Array[String]): Unit = {
    def duoDigits = allDuoDigits//.to(LazyList)
    def smallestDuoDigitMultipleOf(n: Int) = {
      duoDigits.find(_ % n == 0).get
    }

    def sum(n: Int) = {
      (1 to n).par.map { i =>
        val ret = smallestDuoDigitMultipleOf(i)
        println(s"$i -> $ret")
        ret
      }.sum
    }

    println(sum(50000))
  }
}
