object Solution {
  def maxScore(s: String): Int = {
    (0 until s.length())
      .foldLeft(0)((out, idx) => {
        val left = s.substring(0, idx)
        val right = s.substring(idx)

        println(f"${left} and ${right}")

        val l =
          left.foldLeft(0)((acc, char) => if (char == '0') acc + 1 else acc)
        val r =
          right.foldLeft(0)((acc, char) => if (char == '1') acc + 1 else acc)

        println(f"${l} and ${r}; out ${out}")
        
        if (l + r > out) l + r
        else out
      })
  }
}