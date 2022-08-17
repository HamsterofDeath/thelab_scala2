package hod.euler

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._

object Euler714 {
  private  val replacements = {
    (0 to 9 flatMap { first =>
      0 to 9 map { second =>
        List(first, second)
      }
    }).distinct
      .map { e => (e.head.toString.head, e.last.toString.head) }
  }

  private  val cache = mutable.HashMap.empty[Int, Array[BigInt]]
  private  val maxDigitsForCache = 15

  def evalOrFromCache(digits:Int) = {
    def eval = {
      val min = Integer.parseInt("1".padTo(digits, '0'), 2)
      val max = Integer.parseInt("1".padTo(digits, '1'), 2)
      (min to max).iterator.flatMap { dec =>
        forBinaryString(dec.toBinaryString)
      }
    }
    if (digits <= maxDigitsForCache) {
      cache.getOrElseUpdate(digits, {
        eval.distinct.toArray
      }).iterator
    } else {
      eval
    }
  }



  def allDuoDigits = {
    Iterator.from(1).map(evalOrFromCache)
  }

  def forBinaryString(binaryString:String) = {
    replacements
    .iterator
      .filter { case (zero, one) =>
      binaryString.head match {
        case '0' => zero != '0'
        case '1' => one != '0'
      }}
      .map { case (zero, one) =>
      binaryString.map {
        case x if x == '0' => zero
        case x if x == '1' => one
      }
    }.filter(_.head != '0')
      .map { e =>
        BigInt(e)
      }
  }

  def main(args: Array[String]): Unit = {
    def duoDigits = allDuoDigits//.to(LazyList)
    def smallestDuoDigitMultipleOf(n: Int) = {
      duoDigits.map { set =>
        set.filter(_ % n == 0)
      }.filter(_.nonEmpty)
        .map(_.min)
        .next()
    }

    def sum(n: Int) = {
      val counter = new AtomicInteger()
      (1 to n).par.map { i =>
        val ret = smallestDuoDigitMultipleOf(i)
        if (counter.incrementAndGet()%1000==0) {
          print(".")
        }

        ret
      }.sum
    }

    measured {
      (1 to maxDigitsForCache).foreach(evalOrFromCache)
      println(sum(50000))
    }

  }
}
