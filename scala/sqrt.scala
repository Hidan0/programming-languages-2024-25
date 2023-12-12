object Sqrt {
  val EPSILON = 0.0001

  def sqrt(x: Double): Double = {
    def abs(x: Double) = if (x >= 0) x else -x
    def isGood(guess: Double, x: Double): Boolean = abs(guess * guess - x) < EPSILON
    def sqrt(guess: Double, x: Double): Double = 
      if (isGood(guess, x)) guess 
      else sqrt((guess + x / guess) / 2, x)

    sqrt(x, x) 
  }
}
