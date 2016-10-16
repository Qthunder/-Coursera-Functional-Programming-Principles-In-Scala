package recfun

import com.sun.javaws.exceptions.InvalidArgumentException

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
    def pascal(c: Int, r: Int): Int = (c,r) match {
      case (0 , _ ) => 1
      case _ => {
        if (c == r)
          1
        else if (c < r)
          pascal (c - 1, r - 1) + pascal(c, r - 1)
        else
          throw new InvalidArgumentException(Array("Entry not part of table"))
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balance(chars: List[Char], numberOfOpenBrackets : Int) : Boolean =
        if (numberOfOpenBrackets < 0)
          false
        else chars match {
          case Nil => numberOfOpenBrackets == 0
          case ('(' :: tail) => balance(tail, numberOfOpenBrackets + 1)
          case (')' :: tail) => balance(tail, numberOfOpenBrackets - 1)
          case (_ :: tail) => balance(tail, numberOfOpenBrackets)
      }

      balance(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
      case (0, _ ) => 1
      case (_ , Nil) => 0
      case _ if money < 0 => 0
      case (_, (firstCoin::otherCoins)) => countChange(money - firstCoin, coins) + countChange(money, otherCoins)
    }
  }
