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
    )

    0 to 9 foreach { n =>
      val active = activeBars(n)
      def charOrNot(position:Int) = if (active.contains(position)) "X" else " "
      val debug = s"""
         |${charOrNot(1)}${charOrNot(1)}${charOrNot(1)}
         |${charOrNot(4)} ${charOrNot(5)}
         |${charOrNot(2)}${charOrNot(2)}${charOrNot(2)}
         |${charOrNot(6)} ${charOrNot(7)}
         |${charOrNot(3)}${charOrNot(3)}${charOrNot(3)}
         |""".stripMargin
      println(debug)
    }

    def barsOf(n: Int) = activeBars(n)
    def digitSwitchCost(from: Int, to: Int) = {
      val barsFrom = barsOf(from)
      val barsTo = barsOf(to)
      barsFrom.diff(barsTo).size + barsTo.diff(barsFrom).size
    }
    def onOffCost(n: Int): Int = barsOf(n).size
    def digitalRoots(start: Long): Iterator[Long] = {
      var cursor = start
      Iterator.single(start) ++ Iterator
        .continually {
          val root = cursor.toString.map(_.getNumericValue).sum
          cursor = root
          root.toLong
        }
        .takeWhilePlusOne(_ > 10)
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
      val allDigitalRoots = digitalRoots(start).toList
      allDigitalRoots.sliding(2).foreach { seq =>
        val addCost =
          if (optimized) totalSwitchCostOptimized(seq.head, seq.last)
          else totalSwitchCost(seq.head, seq.last)
        sum += addCost
      }
      sum += totalOnOffCost(allDigitalRoots.last)
      sum
    }

    summedSwitchCosts(201,true)

    val start = 10000000
    val end = start*2

    val primes = allPrimesLong.dropWhile(_ < start).takeWhile(_ <= end).toList
    val solution = primes.map { prime =>
      summedSwitchCosts(prime, false) - summedSwitchCosts(prime, true)
    }.sum
    println(solution)
  }
}
