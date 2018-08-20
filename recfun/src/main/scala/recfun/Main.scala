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
        if(c < 0 || r < 0 || c > r) 0
        else if (c == 0 || c == r) 1
        else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def innerbalance(numOfParens: Int, innerChars: List[Char]) : Boolean = {
        if(innerChars.isEmpty) {
          if(numOfParens == 0) return true else false
        }

        if(numOfParens < 0) return false // ')' before '('

        if(innerChars.head equals '(') innerbalance(numOfParens + 1, innerChars.tail)
        else if(innerChars.head equals ')') innerbalance(numOfParens - 1, innerChars.tail)
        else innerbalance(numOfParens, innerChars.tail)
      }

      innerbalance(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0) 1
      else if(money < 0 || coins.isEmpty) 0
      else
        countChange(money, coins.tail) + countChange(money-coins.head, coins)

    }
  }
