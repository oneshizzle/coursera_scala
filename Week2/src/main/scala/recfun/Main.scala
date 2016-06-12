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
    if (c == 0) {
      1
    } else if (c == r) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val stack = scala.collection.mutable.Stack[Char]()

    def isBalanced(chars: List[Char]): Boolean = {
      if (chars.isEmpty) {
        if (stack.isEmpty) true else false
      } else {
        chars.head match {
          case '(' =>
            stack.push(chars.head); isBalanced(chars.tail)
          case ')' => {
            if (stack.isEmpty) {
              false
            } else {
              stack.pop
              isBalanced(chars.tail)
            }
          }
          case _ => isBalanced(chars.tail)
        }
      }
    }
    isBalanced(chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    } else if (money < 0 || coins.isEmpty) {
      0
    } else {
      countChange(money - coins.head, coins) +
        countChange(money, coins.tail)
    }
  }
}
