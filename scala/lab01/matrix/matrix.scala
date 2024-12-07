class Matrix(val n: Int, val m: Int) {
  private val matrix = Array.ofDim[Int](n, m)

  def set(i: Int, j: Int, v: Int): Unit = {
    require(i >= 0 && i < n && j >= 0 && j < m, "Index out of bound")
    matrix(i)(j) = v
  }

  def fill(values: Seq[Int]): Unit = {
    require(values.length == n * m, s"Expected ${n*m} values but got ${values.length}")
    for {i <- 0 until n; j <- 0 until m}
      matrix(i)(j) = values(i * m + j)
  }

  def display(): Unit = {
    matrix.foreach(row => println(row.mkString(" ")))
  }

  def ≃(that: Matrix): Boolean = {
    require(that.n == n && that.m == m, "Matrices have different size.")
    for {i <- 0 until n; j <- 0 until m}
      if (matrix(i)(j) != that.matrix(i)(j)) return false
    true
  }

  def copy(that: Matrix): Unit = {
    require(that.n == n && that.m == m, "Matrices have different size.")
    for {i <- 0 until n; j <- 0 until m}
      matrix(i)(j) = that.matrix(i)(j)
  }

  def +(that: Matrix): Matrix = {
    require(that.n == n && that.m == m, "Matrices have different size.")
    var out = Matrix(n, m)
    for {i <- 0 until n; j <- 0 until m}
      out.matrix(i)(j) = matrix(i)(j) + that.matrix(i)(j)
    out
  }

  def *(that: Int): Matrix = {
    var out = Matrix copy this
    for {i <- 0 until n; j <- 0 until m}
      out.matrix(i)(j) = matrix(i)(j) * that
    out
  }

  def *(that: Matrix): Matrix = {
    require(that.n == m && that.m == n, "Matrices have different size.")
    var out = Matrix(n, m)
    for (i <- 0 until n; j <- 0 until that.m)
      out.matrix(i)(j) = (0 until this.m).map(k => matrix(i)(k) * that.matrix(k)(j)).sum
    out 
  }

  def transpose(): Matrix = {
    var out = Matrix(m,n)
    for (i <- 0 until n; j <- 0 until m)
      out.matrix(j)(i) = matrix(i)(j)
    out
  }

  def norm1(): Int = {
    (0 until n).map(col =>
        (0 until m).map(row =>
            Math.abs(matrix(row)(col))
            ).sum
          ).max
  }
}

object Matrix {
  def apply(n: Int, m: Int): Matrix = {
    new Matrix(n, m)
  }

  def apply(n: Int): Matrix = {
    new Matrix(n, n)
  }

  def fill(n: Int, m: Int, values: Seq[Int]): Matrix = {
    var mat = Matrix(n, m)
    mat.fill(values)
    mat
  }

  def copy(that: Matrix): Matrix = {
    var mat = Matrix(that.n, that.m)
    mat copy that
    mat
  }
}
