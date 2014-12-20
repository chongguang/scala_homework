package recfun
import common._

/**
 * @author chongguang
 *
 */
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
    
    def pascalIter(c: Int, r: Int): Int = {
      if ( r < c || c < 0 || r < 0 ) 0  else {
        if ( r == c || c == 0 ) 1 else pascalIter( c - 1, r - 1) + pascalIter( c, r - 1)
      }
    }
    
    if ( r < c || c < 0 || r < 0 ) throw new IllegalArgumentException ( "Illegal arguments detected!" )
    else pascalIter( c, r )
    
  }
 
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    
    def isBalanced(chars: List[Char], stack: Int): Boolean = {
      if ( chars.isEmpty ) stack == 0 else
        if ( chars.head.equals( '(' ) ) isBalanced( chars.tail, stack + 1 ) else
          if ( chars.head.equals( ')' ) ) isBalanced( chars.tail, stack - 1 ) && stack > 0 else
            isBalanced( chars.tail, stack )
    }
    
    isBalanced( chars, 0 )
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
    def countChangeIter( money: Int, sortedCoinsList: List[Int]): Int = {
      if ( money == 0 ) 1 else
        if ( money < 0 ) 0 else
          if ( sortedCoinsList.isEmpty ) 0 else
            countChangeIter( money, sortedCoinsList.tail) +
            countChangeIter( money - sortedCoinsList.head, sortedCoinsList)
    }

    def checkCoins(coins: List[Int]): Boolean = {
      if (coins.isEmpty) true else
        coins.head > 0 && checkCoins( coins.tail)
    }
    
    if ( money <= 0 ) throw new IllegalArgumentException ( "The money can NOT be negative or 0!" ) else
      if ( !checkCoins( coins ) ) throw new IllegalArgumentException ( "The value of coins can NOT be negative!" ) else {
        countChangeIter( money, coins.sortWith(_.compareTo(_) < 0) )
      }
    
  }

  /**
   * tail-recursive Factorial function
   */
  def factorial(n: Int):Int = {
    def iterFactorial(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else iterFactorial(n-1, acc * n)
    }
    iterFactorial(n,1)
  }
  
  /**
   * tail-recursive sum of list function
   */
  def sumList(ls: List[Int]): Int = {
    def iter(list: List[Int], acc: Int): Int = {
      if (list.isEmpty) acc
      else iter(list.tail, acc + list.head)
    }
    iter(ls,0)
  }
}
