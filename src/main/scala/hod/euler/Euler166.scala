package hod.euler
import collection.parallel.CollectionConverters._

object Euler166 {
  def main(args: Array[String]): Unit = {
    val solutions = (0 to 27).par.map { sumUpTo =>
      val data = Array.fill(4, 4)(0)

      def generate(row: Int, col: Int): Int = {
        if (row == 4) {
         // println(data.map(_.mkString).mkString("\n")+"\n")
          1
        }
        else {
          val sumInRow =
            col match {
              case 0 => 0
              case 1 => data(row)(0)
              case 2 => data(row)(0)+data(row)(1)
              case 3 => data(row)(0)+data(row)(1)+data(row)(2)
            }
          val sumInCol =
            row match {
              case 0 => 0
              case 1 => data(0)(col)
              case 2 => data(0)(col)+data(1)(col)
              case 3 => data(0)(col)+data(1)(col)+data(2)(col)
            }
          val sumInDiagonal = {
            if (row == col) {
              row match {
                case 0 => 0
                case 1 => data(0)(0)
                case 2 => data(0)(0)+data(1)(1)
                case 3 => data(0)(0)+data(1)(1)+data(2)(2)
              }
            } else if (row == 3 - col) {
              row match {
                case 0 => 0
                case 1 => data(0)(3)
                case 2 => data(0)(3)+data(1)(2)
                case 3 => data(0)(3)+data(1)(2)+data(2)(1)
              }
            } else {
              0
            }
          }
          val openInRow = sumUpTo - sumInRow
          val openInCol = sumUpTo - sumInCol
          val openInDiagonal = sumUpTo - sumInDiagonal

          def minToInsertToReach(has: Int, needs: Int, emptyCells: Int) = {
            val canFillInTheFuture = 9 * emptyCells
            val needsInCurrentCell = needs - has - canFillInTheFuture
            needsInCurrentCell max 0
          }
          def isOnDiagonal = row==col || row == 3-col
          val minToInsertToFillRow =
            minToInsertToReach(sumInRow, openInRow, 3 - col)
          val minToInsertToFillCol =
            minToInsertToReach(sumInCol, openInCol, 3 - row)
          val minToInsertToFillDiagonal =
            if (isOnDiagonal) minToInsertToReach(sumInDiagonal, openInDiagonal, 3 - row) else 0

          val maxLimit = ((openInCol min openInRow min openInDiagonal)) min 9
          val minLimit =
            minToInsertToFillRow max minToInsertToFillCol max minToInsertToFillDiagonal
          (minLimit to maxLimit).map { e =>
            data(row)(col) = e
            def nextRow = if (col == 3) row + 1 else row
            def nextCol = if (col == 3) 0 else col + 1
            val counts = generate(nextRow, nextCol)
            data(row)(col) = 0
            counts
          }.sum
        }
      }
      print(s"Generating solutions for sum $sumUpTo:")
      val total = generate(0, 0)
      println(total)
      total
    }

    val all = solutions.sum
    println(all)
  }
}
