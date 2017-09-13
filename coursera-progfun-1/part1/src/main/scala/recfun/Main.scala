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
    * Pascal triangle element calculation
    */
    def pascal(c: Int, r: Int): Int = {
      if(c > r) throw new IllegalArgumentException()
      else if(c == 0) 1
      else if(c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
    /**
    * Exercise 2
    * Parenthesis balancing in expression
    */
    def balance(chars: List[Char]): Boolean = {
      balanceLoop(chars, 0)
    }

    def balanceLoop(remainingChars: List[Char], parenthesesCount: Integer): Boolean = {
      if(remainingChars.isEmpty) parenthesesCount == 0
      else if(parenthesesCount < 0) false
      else if(remainingChars.head.equals('(')) balanceLoop(remainingChars.tail, parenthesesCount+1)
      else if(remainingChars.head.equals(')')) balanceLoop(remainingChars.tail, parenthesesCount-1)
      else balanceLoop(remainingChars.tail, parenthesesCount)
    }
  
    /**
    * Exercise 3
    * Combinations of change
    */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0) 1
      else if(coins.isEmpty) 0
      else if(coins.head <= money) countChange(money, coins.tail) + countChange(money - coins.head, coins)
      else countChange(money, coins.tail)
    }

  }
