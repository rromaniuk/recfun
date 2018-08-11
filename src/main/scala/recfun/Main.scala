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
    if (c == 0 || r == c) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) true else isBalanced(0, chars)
  }

  def isBalanced(result: Int, xs: List[Char]): Boolean = {
    if (xs.nonEmpty) {
      if (result < 0) false
      else if ("(".equals(xs.head.toString)) isBalanced(result + 1, xs.tail)
      else if (")".equals(xs.head.toString)) isBalanced(result - 1, xs.tail)
      else isBalanced(result, xs.tail)
    } else result == 0
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countWays(money: Int, coins: List[Int]): Int =
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else countWays(money - coins.head, coins) + countWays(money, coins.tail)
      countWays(money, coins)
  }
}
