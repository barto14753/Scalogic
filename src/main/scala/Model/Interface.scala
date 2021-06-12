package Model

import scala.io.StdIn.readLine

class Interface {
  var open = true
  val logo: String = "  _________             .__                .__        " +
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
      println("- operators: \n" +
        "    v - (or)\n" +
        "    ^ - (and)\n" +
        "    ~ - (not)\n" +
        "    > - (implication)")
      println("- closures: (, )")
      println("Example: (~p v q) ^ r")
      println("Type 'quit' to quit\n")
  }
  def run(): Unit =
  {
      println(logo)
      hello()
      while(open)
      {
        var expression = readLine("Expression: ")

        if(expression == "quit") {

          open = false
        }
        else{
          val expressionHandler = new ExpressionHandler(expression)
          println("\n\n")
          expressionHandler.showTruthTable()
          println("\n\n")
        }
      }

  }

}
