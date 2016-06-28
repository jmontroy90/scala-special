package recfun

import sun.font.TrueTypeFont

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
    if (r == c || r == 0 || c == 0) 1 // if we hit the top or sides, we know the triangle has a 1
    else pascal(c-1, r-1) + pascal(c,r-1) // recursively represents: each number is the sum of two numbers above it
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val (left, right) = (0,0)

    def pop(chars: List[Char], left: Int, right: Int): Boolean = {

      if (chars.isEmpty) {
        if (left != right) false // if not balanced lefts and rights
        else true
      }

      else if (right > left) false // if your right parens get ahead, false

      else {
        if (chars.head.toString == "(") pop(chars.tail, left + 1, right) // left
        else if (chars.head.toString == ")") pop(chars.tail, left, right + 1) // right
        else pop(chars.tail, left, right) // other chars
      }

    }

    pop(chars, left, right)

  }




  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    // add your recursive cases - one for the current coin, one for the rest of the coins.
    // kind of a simultaneous crawling of change list.

    if (money == 0) 1 // made change!
    else if (money < 0) 0 // missed making change with current head coin
    else if (coins.isEmpty) 0 // couldn't make change
    else countChange(money, coins.tail) + countChange(money - coins.head, coins) // this is the magic.

  }
}
