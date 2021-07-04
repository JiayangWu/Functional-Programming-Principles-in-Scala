package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || c == r) 1
    else {
      pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = { 
    def balanceHelper(chars: List[Char], leftCnt: Int): Boolean = {
      if (chars.isEmpty) leftCnt == 0
      else {
        if (chars.head == '(') 
          balanceHelper(chars.tail, leftCnt + 1)
        else if (chars.head == ')') 
          leftCnt > 0 && balanceHelper(chars.tail, leftCnt - 1)
        else
          balanceHelper(chars.tail, leftCnt)
      }
    }
  balanceHelper(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
