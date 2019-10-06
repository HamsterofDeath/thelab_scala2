package hod.euler

import scala.collection.mutable

object Euler493 {
  val poolSize                    = 70
  val colors                      = 7
  val picks                       = 20
  val initialOptionsPerColor: Int = poolSize / colors

  case class Snapshot(data: collection.Map[Int, Int])
  class State {
    def snapshot = Snapshot(openAtColor.toMap)
    def preSnapshot = Snapshot(openAtColor)

    def distinctColorCount: Int = touchedColors.size

    def picksLeft: Boolean = remainingPicks > 0

    def remainingOptions: Int = poolSize - (picks - remainingPicks)

    def putBack(color: Int): Unit = {
      openAtColor.put(color, openAtColor(color) + 1)
      untouched(color) = openAtColor(color) == initialOptionsPerColor
      remainingPicks += 1
    }

    def pick(color: Int): Unit = {
      openAtColor.put(color, openAtColor(color) - 1)
      untouched(color) = false
      remainingPicks -= 1
    }

    private val openAtColor    = mutable.HashMap.empty[Int, Int] ++= {
      (0 to colors).map {_ -> initialOptionsPerColor}
    }
    private val untouched      = Array.tabulate(7)(_ => true)
    private var remainingPicks = picks
    def untouchedColors: Iterator[Int] = untouched.iterator.zipWithIndex.filter(_._1).map(_._2)
    def touchedColors: Iterator[Int] = untouched.iterator.zipWithIndex.filterNot(_._1).map(_._2)
    def optionsForNewColor: Int = untouchedColors.size * initialOptionsPerColor
    def optionsForOldColor: Int = touchedColors.map(openAtColor).sum
  }

  def main(args: Array[String]): Unit = {
    val cache = mutable.HashMap.empty[Snapshot, Double]

    def recur(details: State): Double = {
      if (details.picksLeft) {
        def eval = {
          val chanceForNewColorPick = details.optionsForNewColor.toDouble / details.remainingOptions
          val chanceForOldColorPick = details.optionsForOldColor.toDouble / details.remainingOptions

          def evalSubProblem(color: Int): Double = {
            details.pick(color)
            val ret = recur(details)
            details.putBack(color)
            ret
          }

          val untouchedCount = details.untouchedColors.size
          val touchedCount = details.touchedColors.size

          val expectedColorsIfPickingNew = {
            if (untouchedCount > 0) {
              details.untouchedColors.toList.iterator.map(evalSubProblem).sum / untouchedCount
            } else {
              0
            }
          }
          val expectedColorsIfPickingOld = {
            if (touchedCount > 0) {
              details.touchedColors.toList.iterator.map(evalSubProblem).sum / touchedCount
            } else {
              0
            }
          }
          val sumNew = expectedColorsIfPickingNew * chanceForNewColorPick
          val sumOld = expectedColorsIfPickingOld * chanceForOldColorPick
          sumNew + sumOld
        }

        cache.get(details.preSnapshot) match {
          case Some(data) =>
            data
          case None =>
            val num = eval
            cache.put(details.snapshot, num)
            num
        }

      }
      else {
        details.distinctColorCount
      }
    }

    val num = recur(new State)
    println(num)

  }
}
