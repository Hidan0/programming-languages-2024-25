class KWIC {
  val irrilevant = List("the", "of", "to", "but", "over", "about", "for")

  def explodeTitle(title: String, index: Int) = {
    val words = title.split(" ")
    (0 to words.size - 1)
      .filter(pos => !irrilevant.contains(words(pos).toLowerCase()))
      .map(pos => words.splitAt(pos))
      .map({ case (l, r) => (l.mkString(" "), r.mkString(" ")) })
      .map({ case (l, r) =>
        f"${index + 1}%4d ${l.substring(Math.max(l.length(), 33) - 33)}%33s ${r
            .substring(0, Math.min(r.length(), 40))}%-40s"
      })
  }

  def calculateKwic(titles: List[String]) = {
    titles.zipWithIndex
      .map({ case (title, index) => explodeTitle(title, index) })
      .flatten
      .sortWith((t1, t2) => t1.substring(39) < t2.substring(39))
  }
}

object KWIC {
  def main(args: Array[String]) = {
    val titles = scala.io.Source.fromFile(args(0)).getLines()
    val k = new KWIC
    val theKwic = k.calculateKwic(titles.toList)
    theKwic.foreach(println)
  }
}
