import scala.annotation.tailrec

object Factorial {
  def fac(n: Int): Int = {
    @tailrec
    def aux(n: Int, acc: Int): Int = {
      if (n < 1) acc
      else aux(n - 1, acc * n)
    }

    aux(n, 1)
  }
}
