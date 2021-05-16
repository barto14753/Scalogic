package Model

import Interfaces.ExpressionEvaluator
import scala.collection.mutable.Stack


class MyExpressionEvaluator extends ExpressionEvaluator
{
  override def getTruthValue(expression: String) =
    {
      true;
    }

  override def infixToPostfix(infixExpression: String): String = ???

  override def evaluate(postfixExpression: String): Boolean = ???

}

