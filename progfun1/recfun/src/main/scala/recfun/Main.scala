package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r || r == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceRec(left: Int, chars: List[Char]): Boolean = {
      if (left < 0) false
      else if (chars.isEmpty) left == 0
      else if (chars.head == '(') balanceRec(left + 1, chars.tail)
      else if (chars.head == ')') balanceRec(left - 1, chars.tail)
      else balanceRec(left, chars.tail)
    }
    balanceRec(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else {
      // does not use the first denomination
      countChange(money, coins.tail) +
        // use the first denomination at least once
        countChange(money - coins.head, coins)
    }
  }
}
