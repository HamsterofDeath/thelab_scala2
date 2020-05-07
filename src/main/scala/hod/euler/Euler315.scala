package hod.euler

object Euler315 {
  def main(args: Array[String]): Unit = {
    val activeBars = Array(
      List(1, 3, 4, 5, 6, 7),
      List(5, 7),
      List(1, 2, 3, 5, 6),
      List(1, 2, 3, 5, 7),
      List(2, 4, 5, 7),
      List(1, 2, 3, 4, 7),
      List(1, 2, 3, 4, 6, 7),
      List(1, 4, 5, 7),
      List(1, 2, 3, 4, 5, 6, 7),
      List(1, 2, 3, 4, 5, 7)
    ).map { barIds =>
      var bits=0
      barIds.foreach { digit =>
        bits |= 1<<digit
      }
      bits
    }

    def barsOf(n: Int) = activeBars(n)
    def digitSwitchCost(from: Int, to: Int) = {
      val barsFrom = barsOf(from)
      val barsTo = barsOf(to)
      Integer.bitCount(barsFrom ^ barsTo)
    }
    def onOffCost(n: Int): Int = Integer.bitCount(barsOf(n))
    def digitalRoots(start: Long): Iterator[Long] = {
      var cursor = start
      Iterator.single(start) ++ Iterator
        .continually {
          val root = cursor.toString.map(_.getNumericValue).sum
          cursor = root
          root.toLong
        }
        .takeWhilePlusOne(_ >= 10)
    }

    def totalOnOffCost(n: Long): Long = {
      n.allDigitsReversed.map(onOffCost).sum
    }

    def totalSwitchCost(from: Long, to: Long): Long = {
      totalOnOffCost(from) + totalOnOffCost(to)
    }

    def totalSwitchCostOptimized(from: Long, to: Long): Long = {
      from.allDigitsReversed
        .zipAll(to.allDigitsReversed, -1, -1)
        .map {
          case (digitA, digitB) =>
            if (digitB == -1) {
              onOffCost(digitA)
            } else {
              digitSwitchCost(digitA, digitB)
            }
        }
        .sum
    }

    def summedSwitchCosts(start: Long, optimized: Boolean) = {
      var sum = totalOnOffCost(start)
      val allDigitalRoots = digitalRoots(start)
      var last = -1L
      allDigitalRoots.sliding(2).foreach { seq =>
        val addCost =
          if (optimized) totalSwitchCostOptimized(seq.head, seq.last)
          else totalSwitchCost(seq.head, seq.last)
        sum += addCost
        last = seq.last
      }
      sum += totalOnOffCost(last)
      sum
    }

    val start = 10000000
    val end = start * 2

    val solution = measured {
      val primes = allPrimesLong.dropWhile(_ < start).takeWhile(_ <= end)
      val solution = primes.map { prime =>
        summedSwitchCosts(prime, false) - summedSwitchCosts(prime, true)
      }.sum
      solution
    }
    println(solution)
  }
}
