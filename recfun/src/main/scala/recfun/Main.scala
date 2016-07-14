package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(countChange(4, List(1,2)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    c match {
      case 0 => 1
      case `r` => 1
      case _ => pascal(c-1, r-1) + pascal(c, r-1)
    }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def countBalance(): Boolean =
      chars.count(_ == '(') == chars.count(_ == ')')

    def orderBalance(): Boolean = {
      def checkOrder(acc: List[Char], chars: List[Char]): Boolean = {
        chars match {
          case Nil => true
          case h::rest => h match {
            case '(' => checkOrder(h::acc, rest)
            case ')' => if (acc.nonEmpty && acc.head == '(') checkOrder(acc.tail, rest) else false
            case _ => checkOrder(acc, rest)
          }
        }
      }

      checkOrder(List[Char](), chars)
    }

    countBalance() && orderBalance()
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    // Action to bind to previous state
    def add(currentMoney: List[Int], coins: List[Int]): List[List[Int]] =
      coins.map(x => if (currentMoney.sum < 4) (x::currentMoney).sortWith(_<_) else currentMoney)

    // Start counting
    def count(res: List[List[Int]]): Int =
      if (res.exists(_.sum < 4))
        count(res.flatMap(add(_, coins)).distinct)
      else
        res.count(_.sum == 4)

    count(List(List()))
  }
}

