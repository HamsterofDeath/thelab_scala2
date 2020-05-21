package hod.training

object Initiate {
  def main(args: Array[String]): Unit = {
    val minutes = 24*60
    val solved = (0 to minutes).map { time =>
      val hours = time / 60
      val minutes = time % 60

      val ret = (hours.toString+minutes.toString).exists(_ == '1')
      if (ret) println(hours+":"+minutes)
      if (ret) 1 else 0
    }.sum
    println(solved)
  }
}
