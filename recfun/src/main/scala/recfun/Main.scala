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

      def countBalance(chars: List[Char]): Boolean =
        chars.count(_ == '(') == chars.count(_ == ')')

      def orderBalance(chars: List[Char]): Boolean = {

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

    countBalance(chars) && orderBalance(chars)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
