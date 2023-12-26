object AFewMoreMath {
  def isPrime(n: Int): Boolean = {
    !((2 until n - 1) exists (n % _ == 0))
  }

  def goldbach(n: Int): List[(Int, Int)] = {
    val primes = (2 until n - 1).filter(isPrime(_)).toList

    for {
      x <- primes
      y <- primes
      if x + y == n
    } yield (x, y)
  }

  def goldbach(n: Int, m: Int): List[(Int, Int)] = {
    (n to m)
      .filter(x => x > 2 && x % 2 == 0)
      .toList
      .map(x => goldbach(x))
      .flatten
  }
}
