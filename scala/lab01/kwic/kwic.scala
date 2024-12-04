object KWIC {
  val LEFT_MAX: Int = 33
  val RIGHT_MAX: Int = 40

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("*** Err :- Provide file name")
      return ()
    }

    val titles = io.Source.fromFile(args(0)).getLines().toArray
    val words: Seq[(String, Int)] = titlesToIndexedWords(titles.toSeq)
    val sortedWords = words.sortBy(word  => word._1)

    sortedWords.foreach((w) => {
      val (word, idx) = w
      val title = titles(idx)
      val wordAt = title.indexOf(word)
      val rightPart = String.format(s"%-${RIGHT_MAX}s", title.slice(wordAt,title.length).trim().take(RIGHT_MAX))
      val leftPart = String.format(s"%${LEFT_MAX}s", sliceAndTrimLeft(title, wordAt-1, LEFT_MAX).trim())
      println(f"${idx+1}%4d $leftPart $rightPart")
    })
  }

  def titlesToIndexedWords(lines: Seq[String]): Seq[(String, Int)] = {
    lines.zipWithIndex
      .foldLeft(Seq.empty[(String, Int)]){
        (Acc, PackedTitle) => {
          val Title = PackedTitle._1
          val Idx = PackedTitle._2
          val res = Title.split(" ").filterNot(isMinor)
          val words = res.zip(Seq.fill(res.length)(Idx)).toSeq
          words ++ Acc
        }}
  }

  def isMinor(word: String): Boolean = {
    val articles = Seq("a", "an", "the")
    val conjunctions = Seq(
      "and", "but", "or", "so", "yet", "nor", "for", "after", "although", "as",
      "because", "before", "if", "once", "since", "that", "though", "till", "unless",
      "while", "where", "whether", "because of", "in order that", "even though", 
      "as long as", "as soon as", "just as", "so that", "in case", "now that", 
      "as if", "provided that", "whereas", "inasmuch as", "whenever", "until", 
      "while", "after all", "as though", "lest", "regardless", "apart from", 
      "given that", "if only", "in case that", "in spite of", "on the condition that", 
      "only if", "supposing", "as far as", "in the event that", "not to mention", 
      "rather than", "such that", "to the extent that", "although", "despite", 
      "much as", "whether or not", "assuming that", "besides", "conversely", 
      "except that", "in order to", "like", "provided", "save that", "that is to say", 
      "to the end that", "wherever", "whiles", "by the time", "even if", 
      "on condition that", "so long as", "apart from that", "even when", 
      "if then", "in as much as", "in spite of the fact that", "in the same way that", 
      "not only but also", "notwithstanding", "presuming that", "rather", 
      "seeing that", "unless and until", "whereas as", "whether or no", 
      "as against", "as well as", "in accordance with", "in addition to", 
      "in relation to", "in the light of", "not to speak of", 
      "regardless of the fact that", "so as to", "with regard to"
    )
    val prepositions = Seq(
      "about", "above", "across", "after", "against", "among", "around", "at", 
      "before", "behind", "below", "beside", "between", "by", "down", "during", 
      "for", "from", "in", "inside", "into", "near", "of", "off", "on", "out", 
      "over", "through", "to", "toward", "under", "up", "with"
    )
    val minorWords = articles ++ conjunctions ++ prepositions
    minorWords.contains(word.toLowerCase)
  }

  def sliceAndTrimLeft(s: String, from: Int, max: Int) = {
    val sliced = s.take(from)
    if (sliced.length > max)
      sliced.takeRight(max)
    else
      sliced
  }
}
