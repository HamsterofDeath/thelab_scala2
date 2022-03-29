package hod.euler

import scala.jdk.CollectionConverters._

object Euler151 {
  def main(args: Array[String]): Unit = {
    case class SheetConfig(
        a1: Int,
        a2: Int,
        a3: Int,
        a4: Int,
        a5: Int
    ) {
      def singleSheetValue = if (a1 + a2 + a3 + a4 + a5 == 1) 1.0 else 0.0

      def depth = "".padTo(16 - area, ' ')

      def isLastMove = a1 + a2 + a3 + a4 == 0 && a5 == 1

      require(a1 >= 0)
      require(a2 >= 0)
      require(a3 >= 0)
      require(a4 >= 0)
      require(a5 >= 0)

      def scenariosAfterRandomPick = {
        if (!isLastMove) {
          ((0 until a5).map(_ => split(5)) ++
            (0 until a4).map(_ => split(4)) ++
            (0 until a3).map(_ => split(3)) ++
            (0 until a2).map(_ => split(2)) ++
            (0 until a1).map(_ => split(1)))
        } else {
          Nil
        }
      }

      def split(a: Int) = {
        val ret = a match {
          case 5 => copy(a5 = a5 - 1)
          case 4 => copy(a4 = a4 - 1, a5 = a5 + 1)
          case 3 =>
            copy(
              a3 = a3 - 1,
              a4 = a4 + 1,
              a5 = a5 + 1
            )
          case 2 =>
            copy(
              a2 = a2 - 1,
              a3 = a3 + 1,
              a4 = a4 + 1,
              a5 = a5 + 1
            )
          case 1 =>
            copy(
              a1 = a1 - 1,
              a2 = a2 + 1,
              a3 = a3 + 1,
              a4 = a4 + 1,
              a5 = a5 + 1
            )
        }
        require(area == ret.area + 1)
        ret
      }

      def area = a1 * 16 + a2 * 8 + a3 * 4 + a4 * 2 + a5

    }

    val hack = collection.mutable.HashMap.empty[SheetConfig, Double]
    def recur(scenario: SheetConfig): Double = {
      def evaluate: Double = {
        val options =
          scenario.scenariosAfterRandomPick

        if (options.nonEmpty) {
          scenario.singleSheetValue + options.map(recur).sum / options.size
        } else 0.0
      }

      hack.getOrElseUpdate(scenario, evaluate)
      //evaluate
    }

    val initial = SheetConfig(1, 0, 0, 0, 0).split(1)
    ///val initial = SheetConfig(0, 0, 1, 0, 0, false).split(3)
    val expected = recur(initial)
    println(expected)
  }
}
