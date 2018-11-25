package hod

object Grid {
  def main(args: Array[String]): Unit = {
    def valueAt(x:Int, y:Int) = {
      require(x >= 0)
      require(x < 24, s"x >= 24")
      require(y >= 0)
      require(y < 24, s"y >= 24")
      y*24+(x+1)
    }

    val gridSize = 7

    def sumOfEdges(ulX:Int, ulY:Int) = {
      valueAt(ulX, ulY) +
      valueAt(ulX, ulY + gridSize) +
      valueAt(ulX + gridSize, ulY + gridSize) +
      valueAt(ulX + gridSize, ulY)
    }

    val maxStart = 24 - gridSize
    val solution = {
      for (x <- 0 until maxStart;
           y <- 0 until maxStart if sumOfEdges(x,y) == 1646)
        yield valueAt(x+ gridSize, y + gridSize)
    }
    println(solution.mkString("\n"))
  }
}
