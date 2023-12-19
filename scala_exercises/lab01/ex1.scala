object Ex1 {
  def is_palindrome(str: String): Boolean = {
    var parsed = str.map((c: Char) => c.toLower).filter((c: Char) => c.isLetter)
    parsed == parsed.reverse
  }

  def is_an_anagram(str: String, dict: List[String]): Boolean = {
    dict
      .map((s: String) => s.toSeq.sorted.unwrap)
      .filter((s: String) => s.equals(str.toSeq.sorted.unwrap))
      .lengthIs >= 1
  }
}

object Main extends App {
  List("detartrated", "Do geese see God?", "Rise to vote, sir.", "nope.")
    .map((input: String) =>
      f"is_palindrome(\"$input\") = ${Ex1.is_palindrome(input)}\n"
    )
    .foreach(print)

  val dict =
    List("incerta", "trincea", "cartine", "citarne", "pratesi", "espatrio", "tar")

  List("", "carenti", "sparite", "luigi", "rat")
    .map((input: String) =>
      f"is_an_anagram(\"$input\") = ${Ex1.is_an_anagram(input, dict)}\n"
    )
    .foreach(print)
}
