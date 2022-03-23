package hod.euler

object Euler121 {
  def main(args: Array[String]): Unit = {
    val maxDepth = 15
    val blueDiscsOnLevel = 1
    case class GameOutcome(gameWon:Long, gameCount:Long) {
      def *(factor: Int) = GameOutcome(gameWon*factor, gameCount*factor)

      def +(other:GameOutcome): GameOutcome = GameOutcome(gameWon+other.gameWon, gameCount+other.gameCount)
    }
    def playGame(redCollected:Int, blueCollected:Int):GameOutcome = {
      val currentDepth = redCollected + blueCollected
      val redDiscsOnLevel = currentDepth+1
      if (currentDepth<maxDepth) {
        val left = playGame(redCollected + 1, blueCollected)
        val right = playGame(redCollected, blueCollected + 1) * redDiscsOnLevel
        left + right
      } else {
        if (redCollected>blueCollected) {
          GameOutcome(1,1)
        } else {
          GameOutcome(0,1)
        }
      }
    }
    val solution = playGame(0,0)
    val maxFund = solution.gameCount / solution.gameWon
    println(solution)
    println(maxFund)
  }
}
