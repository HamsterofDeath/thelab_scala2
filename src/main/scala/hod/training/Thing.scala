package hod.training

object Thing {
  def main(args: Array[String]): Unit = {
    case class Triplet(game1:Boolean, game2:Boolean, game3:Boolean) {
      def print = List(game1, game2, game3).map(_.toString.reverse.padTo(7,' ').reverse).mkString(", ")

      def countTrue = List(game1, game2, game3).count(_ == true)

    }
    case class ConfigurationForTeam(wonGames:Triplet, tiedGames:Triplet, lostGames:Triplet) {
      def print = List(wonGames, tiedGames, lostGames).map(_.print).mkString(", ")

      def obtainedPoints = {
        wonGames.countTrue * 3 +  tiedGames.countTrue
      }

      def valid = {
        List(wonGames.game1,tiedGames.game1, lostGames.game1).count(_ == true)  == 1&&
          List(wonGames.game2,tiedGames.game2, lostGames.game2).count(_ == true)  == 1 &&
          List(wonGames.game3,tiedGames.game3, lostGames.game3).count(_ == true)  == 1
      }
    }

    val allBools = List(true, false)
    val allTriplets = for {
      a <- allBools
      b <- allBools
      c <- allBools
    } yield Triplet(a,b,c)

    val allConfigurations = (for {
      won <- allTriplets
      tied <- allTriplets
      lost <- allTriplets
    } yield (ConfigurationForTeam(won, tied, lost))).filter(_.valid)

    case class MatchResult(teamASetup:ConfigurationForTeam, teamBSetup:ConfigurationForTeam) {
      def print = {
         teamASetup.print + ", "+teamBSetup.print 
      }

      def teamAScore = teamASetup.obtainedPoints
      def teamBScore = teamBSetup.obtainedPoints+7
      def teamAWon = teamAScore>teamBScore
    }
    val teamBWinScenarios = (for {
      teamAResult <- allConfigurations
      teamBResult <- allConfigurations
    } yield {
      MatchResult(teamAResult, teamBResult)
    }).filter(_.teamAWon)

    val output = teamBWinScenarios.map(_.print).sorted.mkString("\n")
    println(List("AW1","AW2","AW3","AT1","AT2","AT3","AL1","AL2","AL3",
      "BW1","BW2","BW3","BT1","BT2","BT3","BL1","BL2","BL3").map(_.reverse.padTo(7,' ').reverse).mkString(", "))
    println(output)
  }
}
