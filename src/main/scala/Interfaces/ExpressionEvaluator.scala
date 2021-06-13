package Interfaces

trait ExpressionEvaluator {
  def getTruthValue(expression: String): Boolean
  def infixToPostfix(infixExpression: String): String
  def evaluate(postfixExpression: String): Boolean
}
