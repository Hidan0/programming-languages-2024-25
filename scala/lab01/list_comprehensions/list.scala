object ListComp {
  def squaredNumbers(lst: List[Any]): List[Any] = {
    for {n <- lst if (n.isInstanceOf[Int] || 
      n.isInstanceOf[Float] || n.isInstanceOf[Double] 
      || n.isInstanceOf[List[Any]])}
    yield {
      if (n.isInstanceOf[List[Any]]) {
        squaredNumbers(n.asInstanceOf[List[Any]])
      } else if (n.isInstanceOf[Int]) {
        Math.pow(n.asInstanceOf[Int].toDouble, 2.0)
      } else if (n.isInstanceOf[Float]) {
        Math.pow(n.asInstanceOf[Float].toDouble, 2.0)
      } else {
        Math.pow(n.asInstanceOf[Double], 2.0)
      }
    }
  }

  def intersect[T](lst1: List[T], lst2: List[T]): List[T] = {
    for {el <- lst1 if lst2.contains(el)}
    yield el 
  }

  def symmetricDifference[T](lst1: List[T], lst2: List[T]): List[T] = {
    for {
      el <- lst1 ::: lst2 
      if !(lst1.contains(el) && lst2.contains(el))
    } yield el
  }
}

assert(ListComp.squaredNumbers(1 :: "hello" :: 100 :: 3.14 :: ('a'::10::Nil) :: 'c' :: (5::7::'a'::Nil) :: Nil) ==
  List(1.0, 10000.0, 9.8596, List(100), List(25, 49)))
assert(ListComp.intersect(List(1,2,3,4,5), List(4,5,6,7,8)) == List(4,5))
assert(ListComp.symmetricDifference(List(1,2,3,4,5), List(4,5,6,7,8)) == List(1,2,3,6,7,8))
