object Sqrt {
  val EPSILON = 0.0001

  private def abs(x: Double) = if (x >= 0) x else -x
  private def isGood(guess: Double, x: Double): Boolean = abs(guess * guess - x) < EPSILON
  private def sqrt(guess: Double, x: Double): Double = if (isGood(guess, x)) guess else sqrt((guess + x / guess) / 2, x)

  def sqrt(x: Double): Double = sqrt(x, x) 
}
