object FunctionalScala {
  def isPalindrome(str: String): Boolean = {
    val parsed = str.filter(_.isLetter).map(_.toLower)
    parsed == parsed.reverse
  }

  def isAnAnagram(str: String, dict: List[String]): Boolean = {
    dict.map(_.sorted).find((x) => x == str.sorted).isDefined
  }

  def factors(n: Int): List[Int] = {
    def isPrime(num: Int): Boolean = {
      Seq.range(2, num).find(x => num % x == 0).isEmpty
    }
    1 +: Seq.range(2, n+1).filter(x => n % x == 0 && isPrime(x)).toList
  }

  def isProper(n: Int): Boolean = {
    Seq.range(1, n).filter(x => n % x == 0).sum == n 
  }
} 

assert(FunctionalScala.isPalindrome("Do geese see God?") == true)
assert(FunctionalScala.isPalindrome("Rise to vote, sir.") == true)
assert(FunctionalScala.isPalindrome("Anna") == true)
assert(FunctionalScala.isPalindrome("pippo") == false)

assert(FunctionalScala.isAnAnagram("ratto", List("torta", "torto", "trota")) == true)
assert(FunctionalScala.isAnAnagram("ciao", List("torta", "torto", "trota")) == false)

assert(FunctionalScala.factors(125) == List(1, 5))
assert(FunctionalScala.factors(25) == List(1, 5))
assert(FunctionalScala.factors(7) == List(1, 7))
assert(FunctionalScala.factors(64) == List(1, 2))

assert(FunctionalScala.isProper(6) == true)
assert(FunctionalScala.isProper(7) == false)
