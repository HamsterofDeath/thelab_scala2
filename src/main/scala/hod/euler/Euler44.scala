package hod.euler

object Euler44 {
  def main(args: Array[String]): Unit = {
    def penta(n: Long) = (n * (3 * n - 1)) / 2

    def reverse(n: Long) = 1.0 / 6 * (math.sqrt(24 * n + 1) + 1)

    def isPenta(n: Long) = reverse(n).isNatural
    var right = 1L
    var found = false
    case class Found(left: Long, right: Long) {
      def diff = right - left

    }
    var best = Found(0, Long.MaxValue)
    while (!found) {
      var startLeft = right - 1
      while (startLeft > 0) {
        val pLeft = penta(startLeft)
        val pRight = penta(right)
        if (isPenta(pLeft + pRight) && isPenta(pRight - pLeft)) {
          var next = Found(pLeft, pRight)
          if (next.diff < best.diff) {
            best = next
            found = true
          }
        }
        startLeft -= 1
      }
      right += 1
    }
    println(best.diff)

  }
}
