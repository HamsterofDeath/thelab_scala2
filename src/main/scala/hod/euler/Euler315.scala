package hod.euler

object Euler315 {
  def main(args: Array[String]): Unit = {
    def digitalRoots(start:Long) = {
      var cursor = start
      Iterator.single(start) ++ Iterator.continually {
        val root =  cursor.toString.map(_.getNumericValue).sum
        cursor = root
        root
      }.takeWhilePlusOne(_ > 10)
    }

    val start = math.pow(10,7).toLong
    val end = math.pow(10,7).toLong*2
    val primes = allPrimesLong.dropWhile(_ < start).takeWhile(_ < end).toList
    println(primes.map(digitalRoots).map(_.size).max)

  }
}
