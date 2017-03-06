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
    def pascal(c: Int, r: Int): Int = (c,r) match {
      case (0,_) => 1
      case (x,y) if (x==y) => 1
      case (x,y) => pascal(x-1, y-1) + pascal(x, y-1)
    }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val open = '('
    val close = ')'
    def go(ch: List[Char], count: Int): Boolean = ch match {
      case Nil => if (count==0) true else false
      case _ if (count<0) => false
      case h :: t => if (h==open) go(t, count+1) else if (h==close) go(t, count-1) else go(t, count)
    }
    go(chars, 0)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def go(mon: Int, coins: List[Int]): Int = coins match {
      case Nil => 0
      case _ if (mon < 0) => 0
      case _ if (mon==0) => 1
      case h :: t => {
        (for {
          x <- coins
          y = go(mon-x, part(x, coins))
        } yield y).foldRight(0)(_+_)
      }
    }

    def part(c: Int, l: List[Int]): List[Int] = l match  {
      case Nil => Nil
      case h :: t  => if (h == c) l else part(c, t)
    }
    if (money==0 || coins.isEmpty) 0
    else go(money, coins)
  }

}
