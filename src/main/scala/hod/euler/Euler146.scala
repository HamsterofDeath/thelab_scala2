package hod.euler

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.parallel.CollectionConverters.seqIsParallelizable

object Euler146 {
  def main(args: Array[String]): Unit = {
    // i don't know why this works :)
    val checkAdditions = List(1, 3, 7, 9, 13, 27)
    val checkNotPrime = Range(1, 27).filterNot(checkAdditions.contains)
    val certainty = 90 // random guess
    val counter = new AtomicInteger()
    val correctNs = measured {
      Iterator
        .from(10, 10)
        .filterNot { n => n % 3 == 0 || n % 7 == 0 || n % 13 == 0 }
        .takeWhile(_ <= 150000000)
        .map { n =>
          n.toLong * n
        }
        .toList
        .par
        .filter { nSqr =>
          if (counter.getAndIncrement() % 100000 == 0) print('.')
          val bigInt = BigInt(nSqr)
          nSqr % 10 == 0 &&
          checkAdditions.forall { add =>
            (bigInt + add).isProbablePrime(certainty)
          } &&
          checkNotPrime.forall { add =>
            !(bigInt + add).isProbablePrime(certainty)
          }
        }
        .map(_.sqrtNatural)
    }

    println(correctNs.sum)
  }

}
