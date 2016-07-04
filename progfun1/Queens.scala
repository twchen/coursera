class Queens() {
  private def queens(n: Int): Set[List[Int]] = {
    def isSafe(col: Int, queens: List[Int]) = {
      val row = queens.length
      (row - 1 to 0 by -1) zip queens forall {
        case (r, c) => c != col && Math.abs(c - col) != (row - r)
      }
    }
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else {
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
      }
    }
    placeQueens(n)
  }

  private def show(queens: List[Int]) = {
    val lines =
      for (col <- queens.reverse)
        yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }

  def solutions(n: Int) = (queens(n) map show) mkString "\n"
}
