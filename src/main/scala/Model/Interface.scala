package Model

import scala.io.StdIn.readLine

class Interface {
  var open = true

  val logo = "  _________             .__                .__        " +
    "\n /   _____/ ____ _____  |  |   ____   ____ |__| ____  " +
    "\n \\_____  \\_/ ___\\\\__  \\ |  |  /  _ \\ / ___\\|  |/ ___\\ " +
    "\n /        \\  \\___ / __ \\|  |_(  <_> ) /_/  >  \\  \\___ " +
    "\n/_______  /\\___  >____  /____/\\____/\\___  /|__|\\___  >" +
    "\n        \\/     \\/     \\/           /_____/         \\/"

  def hello(): Unit =
  {
      println("\n\n")
      println("Type your expression to get truth table")
      println("You can use:")
      println("- characters as variables")
      println("- operators: v(or), ^(and), ~(not), >(implication)")
      println("- closures: (, )")
      println("Example: (~pvq)^r")
      println("Type 'quit' to quit")
  }
  def run(): Unit =
  {
      println(logo)
      while(open)
      {
        hello()
        var expression = readLine("Expression:")

        if(expression == "quit") {

          open = false
        }
        else{
          val expressionHandler = new ExpressionHandler(expression)
          expressionHandler.showTruthTable()
        }
      }

  }

}
