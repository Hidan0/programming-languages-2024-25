object Ex1 {
  def is_palindrome(str: String): Boolean = {
   var parsed = str.map((c: Char) => c.toLower).filter((c: Char) => c.isLetter)
   parsed == parsed.reverse
  }
}

object Main extends App {
  printf("is_palindrome \"%s\" should be %b, got %b\n", "detartrated", true, Ex1.is_palindrome("detartrated"))
  printf("is_palindrome \"%s\" should be %b, got %b\n", "Do geese see God?", true, Ex1.is_palindrome("Do geese see God?"))
  printf("is_palindrome \"%s\" should be %b, got %b\n", "Rise to vote, sir.", true, Ex1.is_palindrome("Rise to vote, sir."))
  printf("is_palindrome \"%s\" should be %b, got %b\n", "nope", false, Ex1.is_palindrome("nope"))
}
