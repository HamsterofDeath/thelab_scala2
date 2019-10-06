package hod.euler

object GenPrimes {
  def main(args: Array[String]): Unit = {
    allPrimesLong.takeWhile( _ <= 10000000000L).foreach { _ => {} }
  }
}
