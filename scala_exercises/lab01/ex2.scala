class ListComprehensions {
  def intersect(lst1: List[Int], lst2: List[Int]) = {
    for {
      x <- lst1
      if lst2.contains(x)
    } yield x
  }

  def symmetric_difference(lst1: List[Int], lst2: List[Int]) = {
    for {
      x <- lst1 ::: lst2
      if !(lst1.contains(x) && lst2.contains(x))
    } yield x
  }
}

object Mai extends App {
  val lc = new ListComprehensions

  println(f"intersect(\"List(1,2,3,4,5), List(4,5,6,7,8)\") = ${lc
      .intersect(List(1, 2, 3, 4, 5), List(4, 5, 6, 7, 8))}")

  println(f"symmetric_difference(\"List(1,2,3,4,5), List(4,5,6,7,8)\") = ${lc
      .symmetric_difference(List(1, 2, 3, 4, 5), List(4, 5, 6, 7, 8))}")
}
