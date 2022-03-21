package hod.euler
import collection.parallel.CollectionConverters._

object Euler166 {
  def main(args: Array[String]): Unit = {
    val solutions = (0 to 36).par.map { sumUpTo =>
      val data = Array.fill(4, 4)(0)

      def generate(row: Int, col: Int): Int = {
        if (row == 4) {
          1
        } else {
          val sumInCurrentRow =
            col match {
              case 0 => 0
              case 1 => data(row)(0)
              case 2 => data(row)(0) + data(row)(1)
              case 3 => data(row)(0) + data(row)(1) + data(row)(2)
            }
          val sumInCurrentCol =
            row match {
              case 0 => 0
              case 1 => data(0)(col)
              case 2 => data(0)(col) + data(1)(col)
              case 3 => data(0)(col) + data(1)(col) + data(2)(col)
            }
          val sumInDiagonal = {
            if (row == col) {
              row match {
                case 0 => 0
                case 1 => data(0)(0)
                case 2 => data(0)(0) + data(1)(1)
                case 3 => data(0)(0) + data(1)(1) + data(2)(2)
              }
            } else if (row == 3 - col) {
              row match {
                case 0 => 0
                case 1 => data(0)(3)
                case 2 => data(0)(3) + data(1)(2)
                case 3 => data(0)(3) + data(1)(2) + data(2)(1)
              }
            } else {
              0
            }
          }
          val openInRow = sumUpTo - sumInCurrentRow
          val openInCol = sumUpTo - sumInCurrentCol
          val openInDiagonal = sumUpTo - sumInDiagonal

          val maxLimit = openInCol min openInRow min openInDiagonal min 9

          (0 to maxLimit).map { e =>
            data(row)(col) = e
            def nextRow = if (col == 3) row + 1 else row
            def nextCol = if (col == 3) 0 else col + 1
            val counts = {
              val continue = {
                val rowOk = col < 3 || data(row)(0) + data(row)(1) + data(row)(2) + data(row)(3) == sumUpTo
                val colOk = row < 3 || data(0)(col) + data(1)(col) + data(2)(col) + data(3)(col) == sumUpTo
                val diag1Ok = col < 3 || row < 3 || data(0)(0) + data(1)(1) + data(2)(2) + data(3)(3) == sumUpTo
                val diag2Ok = col > 0 || row < 3 || data(0)(3) + data(1)(2) + data(2)(1) + data(3)(0) == sumUpTo
                rowOk && colOk && diag1Ok && diag2Ok
              }
              if (continue) {
                generate(nextRow, nextCol)
              } else {
                0
              }
            }
            data(row)(col) = 0
            counts
          }.sum
        }
      }
      val total = generate(0, 0)
      println(s"Generated solutions for sum $sumUpTo:$total")
      total
    }

    val all = solutions.sum
    println(all)
  }
}
