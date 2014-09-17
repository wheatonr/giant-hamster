package recfun
import common._

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
  def pascal(c: Int, r: Int): Int = 
    if (c<0 || c>r || r<0 ) throw new IllegalArgumentException
    else if (c==0 ||  c==r) 1
    else pascal(c,r-1)+pascal(c-1,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
      def bal(level: Int, chars : List[Char]): Int= {
        if (chars.isEmpty || level<0 ) level
        else if (chars.head == ')') bal(level-1,chars.tail)
        else if (chars.head == '(') bal(level+1,chars.tail)
        else bal(level,chars.tail)
      }
      bal(0,chars)==0
    }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money<0 || coins.isEmpty) 0
    else if (money==0) 1
    else countChange(money,coins.tail) + countChange(money-coins.head,coins) 
    
  }
}
