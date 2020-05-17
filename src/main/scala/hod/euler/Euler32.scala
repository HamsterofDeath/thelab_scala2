package hod.euler

import scala.collection.parallel._

object Euler32 {
  def main(args: Array[String]): Unit = {
    val allDigits     = "123456789"
    val source    = allDigits.toSeq.map(_.getNumericValue)
    val expected  = allDigits.map(_.getNumericValue).toSet
    val solutions = {
      (1 to 4).toParArray
        .flatMap { takeForA =>
          (1 to 4).toParArray
            .flatMap { takeForB =>
              source
                .combinations(takeForA)
                .flatMap(_.permutations)
                .flatMap { a =>
                  val aInt = a.mkString.toLong
                  val remainingDigits = source.filterNot(a.contains)
                  remainingDigits
                    .combinations(takeForB)
                    .flatMap(_.permutations)
                    .flatMap { b =>
                      val product = aInt * b.mkString.toLong
                      val digits = (product.allDigits ++ a ++ b).toList
                      if (digits.size==9 && digits.toSet == expected) {
                        println(s"$aInt * ${b.mkString} = $product")
                        List(product)
                      } else {
                        Nil
                      }
                    }
                }
            }
        }
        .seq
        .toSet
    }

    println(solutions.sum)
  }
}
