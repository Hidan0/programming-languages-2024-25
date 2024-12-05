object Goldbach {
  def goldbach(n: Int): Seq[(Int, Int)] = {
    val primes = getPrimes(n)
    val parts = for {x <- primes; y <- primes if n == x + y} yield (x, y)
    parts
  }

  def goldbach(n: Int, m: Int): Seq[(Int, Seq[(Int, Int)])] = {
    Seq.range(n, m).filter(isEven).map(x => (x, goldbach(x)))
  }

  def getPrimes(n: Int): Seq[Int] = {
    Seq(1) ++ (for {x <- Seq.range(2, n) if isPrime(x)}
    yield x)
  }

  def isPrime(n: Int): Boolean = {
    Seq.range(2, n).filter(x => n % x == 0).isEmpty
  }

  def isEven(n: Int): Boolean = {
    n % 2 == 0
  }
}
