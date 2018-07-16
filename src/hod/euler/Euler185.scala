package hod.euler

import scala.collection.{immutable, mutable}

object Euler185 {

  object Problems {
    val simpleTest = List(
      12345L -> 4,
      99999L -> 1,
      17779L -> 2
    )

    val sample = List(
      70794L -> 0,
      34109L -> 1,
      12531L -> 1,
      90342L -> 2,
      39458L -> 2,
      51545L -> 2
    )

    val test1 = List(
      111111111L -> 1,
      222222222L -> 1,
      333333333L -> 1,
      444444444L -> 1,
      555555555L -> 1,
      666666666L -> 1,
      777777777L -> 1,
      888888888L -> 1,
      999999999L -> 1,
      987654321L -> 9
    )

    val test3 = List(
      10L -> 1,
      12L -> 1,
      13L -> 1,
      14L -> 1,
      15L -> 1,
      16L -> 1,
      17L -> 1,
      18L -> 1,
      19L -> 1
    )

    val test4 = List(
      12L -> 1,
      33L -> 1,
      30L -> 0,
      22L -> 0,
      92L -> 0,
      44L -> 0,
      55L -> 0,
      66L -> 0,
      77L -> 0,
      88L -> 0,
      99L -> 0,
      20L -> 0
    )

    val test5 = List(
      15L -> 1,
      25L -> 1,
      35L -> 1,
      45L -> 1,
      55L -> 1,
      65L -> 1,
      75L -> 1,
      85L -> 1,
      95L -> 1
    )


    val realData = List(
      5616185650518293L -> 2,
      3847439647293047L -> 1,
      5855462940810587L -> 3,
      9742855507068353L -> 3,
      4296849643607543L -> 3,
      3174248439465858L -> 1,
      4513559094146117L -> 2,
      7890971548908067L -> 3,
      8157356344118483L -> 1,
      2615250744386899L -> 2,
      8690095851526254L -> 3,
      6375711915077050L -> 1,
      6913859173121360L -> 1,
      6442889055042768L -> 2,
      2321386104303845L -> 0,
      2326509471271448L -> 2,
      5251583379644322L -> 2,
      1748270476758276L -> 3,
      4895722652190306L -> 1,
      3041631117224635L -> 3,
      1841236454324589L -> 3,
      2659862637316867L -> 2
    )

    val test2 = List(
      //4640261571849533
      1111112112119111L -> 1,
      5616185650518293L -> 2,
      3847439647293047L -> 1,
      5855462940810587L -> 3,
      9742855507068353L -> 3,
      4296849643607543L -> 3,
      3174248439465858L -> 1,
      4513559094146117L -> 2,
      7890971548908067L -> 3,
      8157356344118483L -> 1,
      2615250744386899L -> 2,
      8690095851526254L -> 3,
      6375711915077050L -> 1,
      6913859173121360L -> 1,
      6442889055042768L -> 2,
      2321386104303845L -> 0,
      2326509471271448L -> 2,
      5251583379644322L -> 2,
      1748270476758276L -> 3,
      4895722652190306L -> 1,
      3041631117224635L -> 3,
      1841236454324589L -> 3,
      2659862637316867L -> 2
    )
  }

  case class Sequence(seq: Array[Int]) {
    def describeWithAssumption(assumedCorrectIndexes: collection.Set[Int]): String = {
      assumedCorrectIndexes.toList.sorted.zipWithIndex.map { case (digit, int) =>
        if (assumedCorrectIndexes.contains(int)) digit else "X"
      }.mkString
    }

    def shortDescribe: String = seq.mkString

    override def toString: String = shortDescribe

    def foreachWithMarkedIndices(marked: collection.Set[Int])(function: (Int, Int, Boolean) => Unit): Unit = {
      foreachWithIndex { (digit, index) =>
        function(digit, index, marked.contains(index))
      }
    }

    def foreachWithIndex(function: (Int, Int) => Unit): Unit = {
      var x = 0
      while (x < seq.length) {
        function(seq(x), x)
        x += 1
      }
    }

  }
  case class Constraint(sequence: Sequence, correct: Int) {
    def constrainedOptions(whiteList: WhiteList): Iterator[collection.Set[Int]] = {
      val alreadyUsedUp = whiteList.countOverlapWithAssumedSolution(sequence.seq)
      sequence.seq
      .indices
      .filter { i =>
        whiteList.isWhitelisted(sequence.seq(i), i) && !whiteList.isLocked(i)
      }
      .combinations(correct - alreadyUsedUp)
      .map { ints =>
        mutable.BitSet.empty ++= ints
      }
    }
  }

  class WhiteList(constraints: List[Constraint]) {

    private val length             = constraints.head.sequence.seq.length
    private val possible           = Array.tabulate(length)(_ => mutable.BitSet.empty)
    private val manualOptionsStack = Array.tabulate(length)(_ => mutable.BitSet.empty)
    private var unsolvedSlots      = mutable.BitSet.empty
    private var unassumedSlots     = mutable.BitSet.empty
    private var unsolvableSlots    = mutable.BitSet.empty
    private var returnThisSolution = false

    // initial setup
    0 until length foreach { index =>
      val available = 0 to 9
      available.foreach { digit =>
        whitelist(digit, index)
      }
    }

    def countOverlapWithAssumedSolution(other: Array[Int]): Int = {
      var index = 0
      var same = 0
      while (index < length) {
        if (other(index) == assumedSolution(index)) {
          same += 1
        }
        index += 1
      }
      same
    }

    def markFinalSolution(): Unit = {
      returnThisSolution = true
    }

    def returnThis: Boolean = returnThisSolution

    def isWhitelisted(digit: Int, index: Int): Boolean = {
      possible(index)(digit)
    }

    def explainDetails: String = {
      (0 until length).map { i =>
        possible(i).mkString
      }.mkString(", ")
    }

    def assumedSolutions: immutable.IndexedSeq[Int] = 0 until length map assumedSolution

    def describeState: String = {
      if (solutionFound) "Solution candidate"
      else if (invalid) "Unsolvable"
      else "In progress"
    }

    def lockedIndices: collection.Set[Int] = {
      val ret = mutable.BitSet.empty
      0 until length foreach { i =>
        if (isLocked(i)) {
          ret += i
        }
      }
      ret
    }

    def isLocked(at: Int): Boolean = assumedSolution(at) >= 0

    def assumedSolution(at: Int): Int = {
      val set = possible(at)
      if (set.size == 1) set.head else -1
    }

    def describeCurrentSolution: String = {
      val str = assumedSolutions.map { i =>
        if (i >= 0) i else "X"
      }.mkString
      str
    }

    def markCorrect(digit: Int, index: Int): Unit = {
      val storeForLater = manualOptionsStack(index)
      val whiteListOfColumn = possible(index)
      storeForLater ++= whiteListOfColumn
      whiteListOfColumn.clear()
      whiteListOfColumn += digit
      trackOptions(index)

    }

    def unmarkCorrect(index: Int): Unit = {
      possible(index) ++= manualOptionsStack(index)
      manualOptionsStack(index).clear()
      trackOptions(index)

    }

    private def trackOptions(index: Int): Unit = {
      val remainingOptions = possible(index).size

      if (remainingOptions == 0) {
        unsolvedSlots += index
        unsolvableSlots += index
        unassumedSlots += index
      } else if (remainingOptions == 1) {
        unsolvedSlots -= index
        unsolvableSlots -= index
        unassumedSlots -= index
      } else {
        unsolvedSlots += index
        unsolvableSlots -= index
        unassumedSlots += index
      }
    }

    def blacklist(digit: Int, atPosition: Int): Unit = {
      val bitset = possible(atPosition)
      if (bitset(digit)) {
        bitset -= digit
        trackOptions(atPosition)
      }
    }

    def whitelist(digit: Int, atPosition: Int): Unit = {
      val bitset = possible(atPosition)
      if (!bitset(digit)) {
        bitset += digit
        trackOptions(atPosition)
      }
    }

    def invalid: Boolean = {
      unsolvableSlots.nonEmpty
    }

    def solutionFound: Boolean = {
      unsolvedSlots.isEmpty && unassumedSlots.isEmpty
    }

    def inProgress: Boolean = !solutionFound && !invalid

  }

  def main(args: Array[String]): Unit = {

    def solve(constraints: List[(Long, Int)], debug: Boolean): Unit = {
      val forbiddenFirst = {
        val (allZero, other) = constraints.partition(_._2 == 0)
        allZero ++ other.sortBy(e => (e._2, e._1))

      }
      val initialConstraints = {
        forbiddenFirst.map {
          case (seq, correct) =>
            Constraint(Sequence(seq.toString.toArray.map(_.toString.toInt)), correct)
        }
      }
      val whiteList = new WhiteList(initialConstraints)
      var callCount = 0L

      def solveWithRecursion(): Unit = {

        def recur(remainingConstraints: List[Constraint], depth: Int): Unit = {
          callCount += 1
          if (callCount % 1000000 == 0) print('.')
          val keepGoing = remainingConstraints.nonEmpty && !whiteList.invalid
          if (keepGoing) {
            val constraint = remainingConstraints.head
            val currentSequence = constraint.sequence
            if (constraint.correct == 0) {
              currentSequence.foreachWithIndex(whiteList.blacklist)
              recur(remainingConstraints.tail, depth + 1)
              if (whiteList.returnThis) {
                return
              }
              currentSequence.foreachWithIndex(whiteList.whitelist)
            } else {

              val alreadyMarkedAtThisLevel = whiteList.lockedIndices

              val possibleOnThisLevel = {
                constraint.constrainedOptions(whiteList).toList
              }

              possibleOnThisLevel.foreach {
                assumedCorrectIndexes =>

                  val blacklistedInThisIteration = mutable.BitSet.empty

                  def describeState(reversed: Boolean): Unit = {
                    if (debug) {
                      val label = if (reversed) "Reversed" else "Applied"
                      println {
                        s"${0 to depth map { _ => " " } mkString}" +
                        s"$label ${currentSequence.shortDescribe}," +
                        s"assuming as correct ${currentSequence.describeWithAssumption(assumedCorrectIndexes)}," +
                        s"whitelist is at ${whiteList.describeCurrentSolution}," +
                        s"state ${whiteList.describeState}," +
                        s"details: ${whiteList.explainDetails}"
                      }
                    }
                  }

                  def traverseCurrentSequence(func: (Int, Int, Boolean) => Unit): Unit = {
                    val traverseThis = currentSequence
                    traverseThis.foreachWithMarkedIndices(assumedCorrectIndexes) {
                      (digit, index, marked) =>
                        if (!alreadyMarkedAtThisLevel(index)) {
                          func(digit, index, marked)
                        }
                    }
                  }

                  def setupWhitelist(): Unit = {
                    traverseCurrentSequence {
                      (digit, index, assumedCorrect) =>
                        if (assumedCorrect) {
                          whiteList.markCorrect(digit, index)
                        } else {
                          if (whiteList.isWhitelisted(digit, index)) {
                            blacklistedInThisIteration += index
                            whiteList.blacklist(digit, index)
                          }
                        }
                    }
                  }

                  def resetWhiteList(): Unit = {
                    traverseCurrentSequence {
                      (digit, index, assumedCorrect) =>
                        if (assumedCorrect) {
                          whiteList.unmarkCorrect(index)
                        } else {
                          if (blacklistedInThisIteration(index)) {
                            whiteList.whitelist(digit, index)
                          }
                        }
                    }
                  }

                  setupWhitelist()
                  describeState(false)
                  recur(remainingConstraints.tail, depth + 1)
                  if (whiteList.returnThis) {
                    return
                  } else {
                    resetWhiteList()
                    describeState(true)
                  }

              }
            }
          } else if (whiteList.solutionFound) {
            println(s"\nFound possible solution: ${whiteList.describeCurrentSolution}")
            whiteList.markFinalSolution()
          }
        }

        recur(initialConstraints, 0)
        if (whiteList.returnThis) {
          println(s"Solution after using all constraints: ${whiteList.describeCurrentSolution}")
        }
      }

      solveWithRecursion()

      println(s"Steps made: $callCount")
    }

    solve(Problems.simpleTest, debug = false)
    solve(Problems.sample, debug = false)
    solve(Problems.test1, debug = false)
    solve(Problems.test3, debug = false)
    solve(Problems.test4, debug = false)
    solve(Problems.test5, debug = false)
    solve(Problems.test2, debug = false)
    solve(Problems.realData, debug = false)
  }
}
