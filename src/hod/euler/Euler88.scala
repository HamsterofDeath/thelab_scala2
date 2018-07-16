package hod.euler

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer

object Euler88 {

  class IntVar(var i: Int) {
    def dec(): Unit = {
      i -= 1
    }

    def inc(): Unit = {
      i += 1
    }
  }

  case class Pair(number: Int, count: Int) {
    def format: String = {
      1 to count map { _ => number } mkString " * "
    }

    val sum: Int = number.toInt * count
    val pow: Int = math.pow(number, count).toInt
  }

  case class PartialAttempt(elements: List[Pair]) {
    val length: Int = elements.iterator.map(_.count).sum

    def format: String = elements.sortBy(_.number).map(_.format).mkString(" * ")

    val sum: Int = elements.foldLeft(0)((acc, e) => {
      acc + e.sum
    })

    val product: Int = elements.foldLeft(1)((acc, e) => {
      acc * e.pow
    })

    val missing: Int = product - sum

    def complete = Attempt(missing, this)
  }

  case class Attempt(numberOfOnes: Int, partialAttempt: PartialAttempt) {
    val sumProduct: Int = numberOfOnes + partialAttempt.sum

    def format: String = {
      val leftSide = {
        partialAttempt.format
      }
      val rightSide = partialAttempt.product

      s"ps($length) = $leftSide = $rightSide"
    }

    val length: Int = numberOfOnes + partialAttempt.length
  }

  def attempt(format: Int*): PartialAttempt = {
    val pairs = {
      format
      .groupBy(identity)
      .mapValues(_.size)
      .map { case (number, count) =>
        Pair(number, count)
      }
    }
    PartialAttempt(pairs.toList)
  }

  def attempt(map:Map[Int, Int]): Attempt = {
    PartialAttempt(map.map { case (number, count) =>
      Pair(number, count)
    }.toList).complete
  }

  case class Key(parameters: Int*)

  def findSolutions(target: Int, factors: IndexedSeq[Int]): Traversable[Map[Int, Int]] = {

    new Traversable[Map[Int, Int]] {
      override def foreach[U](f: Map[Int, Int] => U): Unit = {
        val workOn = mutable.HashMap.empty[Int, IntVar]

        def recur(leftOver: Int, pool: IndexedSeq[Int]): Unit = {
          if (leftOver > 1) {
            pool
              .iterator
              .filter { e =>
                e <= leftOver && leftOver % e == 0
              }
            .foreach { nextFactor =>
              val where = pool.indexOf(nextFactor)
              if (where >= 0) {
                val value = pool(where)
                val newLeftOver = leftOver / value
                workOn.getOrElseUpdate(value, new IntVar(0)).inc()
                recur(newLeftOver, pool.drop(where))
                workOn(value).dec()
              }
            }
          } else {
            val solution = {
              workOn
                .mapValues(_.i)
                .view
                .filter(_._2 > 0)
                .force
                .toMap
            }
            f(solution)
          }
        }
        recur(target, factors)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val upperLimit = 12000
    val upperLimitSum = upperLimit * 2
    val lowerLimitSum = 4

    val smallest = mutable.HashMap.empty[Int, Attempt]

    def feed(sample: Attempt) = {
      smallest.synchronized {
        if (sample.length >= 2 && sample.length <= upperLimit) {
          smallest.get(sample.length) match {
            case None => smallest.put(sample.length, sample)
            case Some(old) if old.sumProduct > sample.sumProduct => smallest.put(sample.length, sample)
            case _ => // nop
          }
        }
      }
    }

    def allFactorsFor(sum: Int) = {
      val maxRange = 1 to sum / 2

      maxRange.filter { e =>
        sum % e == 0
      }
    }

    (lowerLimitSum to upperLimitSum).par.foreach { sum =>
      val factors = allFactorsFor(sum).filter(_ > 1)
      val allTargetSums = findSolutions(sum, factors)
      allTargetSums.foreach { combination =>
        feed(attempt(combination))
      }
    }

    val explain = {
      smallest
      .toList
      .sortBy(_._1)
      .map(_._2.format)
      .mkString("\n")
    }
    //prIntln(explain)

    def mpsn(n: Int) = {
      smallest
      .iterator
      .filter(_._1 <= n)
      .map(_._2.sumProduct)
      .toSet
      .sum
    }

    println(mpsn(upperLimit))
  }
}

