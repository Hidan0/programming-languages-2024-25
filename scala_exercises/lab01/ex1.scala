object Ex1 {
  def is_palindrome(str: String): Boolean = {
    var parsed = str.map((c: Char) => c.toLower).filter((c: Char) => c.isLetter)
    parsed == parsed.reverse
  }
}

object Main extends App {
  List("detartrated", "Do geese see God?", "Rise to vote, sir.", "nope.")
    .map((input: String) =>
      f"is_palindrome(\"$input\") = ${Ex1.is_palindrome(input)}\n"
    )
    .foreach(print)
}
