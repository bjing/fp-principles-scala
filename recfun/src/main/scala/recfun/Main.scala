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
    // Check if the counts of opening and closing parentheses match
    def countBalance(): Boolean =
      chars.count(_ == '(') == chars.count(_ == ')')

    // Check if the order of parentheses is correct
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

    // Parentheses in a string are balanced if:
    // 1. the number of opening and closing parentheses are equal
    // 2. the order of opening and closing parentheses are correct
    countBalance() && orderBalance()
  }

  /**
   * Exercise 3
   */
  type MoneyState = List[Int]

  def countChange(money: Int, coins: List[Int]): Int = {
    // Given a list of current states, return a list of all possible states after adding a coin
    // to each current state
    def addCoin(currentMoneyState: MoneyState): List[MoneyState] =
      // The if-else guard statement ensures we only update state with less than specified amount of money
      coins map (x => if (currentMoneyState.sum < money) (x::currentMoneyState) sortWith(_<_) else currentMoneyState)

    // Main counting function
    def count(currentMoneyStates: List[MoneyState]): Int =
      if (currentMoneyStates.exists(_.sum < money))
        // Add a coin to each current state and generate all possible resultant states
        count(currentMoneyStates flatMap addCoin distinct)
      else
        // Get the number of all states that have exactly specified amount of money
        currentMoneyStates.count(_.sum == money)

    // Start counting from no coins, so initial state is an empty List.
    // We wrap the initial state in an outer list (List Monad) in order to apply addCoin action using flatMap
    count(List(List()))
  }
}

