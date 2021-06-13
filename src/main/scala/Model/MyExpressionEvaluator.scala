package Model

import Interfaces.ExpressionEvaluator
import Objects.{Conditions, Symbol}
import javax.management.RuntimeErrorException

import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.convert.ImplicitConversions.`mutableSeq AsJavaList`
import scala.collection.mutable
import scala.collection.mutable.Stack


class MyExpressionEvaluator extends ExpressionEvaluator
{
  override def getTruthValue(expression: String) = {
    evaluate(infixToPostfix(expression));
  }

  def checkIfTwoSymbolsTogether(expression: String) = {
    for(i <- 1 until expression.length())
    {
      val prev = expression.charAt(i - 1).toString;
      val curr = expression.charAt(i).toString;

      if(Symbol.isOperator(prev) && Symbol.isOperator(curr))
      {
        if(!Symbol.isNot(curr)) throw new RuntimeErrorException(null);
      }
    }
  }

  def handleNoneOperator(signs: mutable.Stack[String], operands: mutable.Stack[String], chars: String) = {
    if(Symbol.isLeftClosure(chars))
    {
      signs.push(chars)
    }
    else if(Symbol.isRightClosure(chars))
    {
      while(signs.nonEmpty && !Symbol.isLeftClosure(signs.top.toString)) {
        operands.push(signs.pop());
      }
      if(signs.nonEmpty) signs.pop();

    }
    else
    {
      operands.push(chars);
    }
  }

  def handleSameOperators(signs: mutable.Stack[String], operands: mutable.Stack[String],
                             chars: String, sign: String, whileCondition: (String, String, Int) => Boolean) = {
    var sig = sign
    var flag = 0;
    do
    {
      operands.push(signs.pop)
      if(signs.nonEmpty)
      {
        sig = signs.top.toString;
      }
      else
      {
        flag += 1;
      }
    }while(whileCondition(chars, sig, flag));
    signs.push(chars);
    flag = 0;
  }


  def handleDifferentOperators(signs: mutable.Stack[String], operands: mutable.Stack[String],
                             chars: String, sign: String, ifCondition: (String, String) => Boolean,
                               whileCondition: (String, String, Int) => Boolean) = {
    var sig = sign
    if(ifCondition(chars, sig))
    {
      signs.push(chars);
    }
    else
    {
      var flag = 0;
      do
      {
        operands.push(signs.pop);
        if(signs.nonEmpty)
        {
          sig = signs.pop.toString;
        }
        else
        {
          flag += 1;
        }
      }while(whileCondition(chars, sig, flag));
      signs.push(chars);
      flag = 0;
    }
  }

  def connectStacks(signs: mutable.Stack[String], operands: mutable.Stack[String]): String = {
    var counter = signs.size();
    for(i<-1 until counter+1)
    {
      if(Symbol.isClosure(signs.top.toString))
      {
        throw null
      }
      operands.push(signs.pop());
    }
    var result1 = new Array[String](operands.length);

    for(i <- operands.indices)
    {
      result1(i) = operands.pop()
    }
    var result = ""
    for(i<-result1.length-1 to 0 by -1)
    {
      if(i>0) result ++ " ";
      result = result ++ result1(i);
    }
    result

  }


  override def infixToPostfix(expression: String): String =
  {
    var result: String = "";
    if(expression.isEmpty()) throw null;
    else
    {
      checkIfTwoSymbolsTogether(expression)
      var signs = new mutable.Stack[String];
      var operands = new mutable.Stack[String];

      for(i<- 0 until expression.length())
        {
          var chars = expression.charAt(i).toString;

          if(!Symbol.isOperator(chars)) handleNoneOperator(signs, operands, chars)
          else
            {
              if(signs.isEmpty || Symbol.isLeftClosure(signs.top.toString))
                {
                  if(signs.nonEmpty && signs.top.equals(chars)) throw null
                  signs.push(chars);

                }
              else
                {
                  var sign = signs.top.toString;
                  if(chars == " ") { } // continue
                  else
                    {
                      if(Symbol.isNot(chars)) signs.push(chars)
                      else if(Symbol.isAnd(chars) && !Symbol.isAnd(sign)) // ---------- AND -------------
                        {
                          handleDifferentOperators(signs, operands, chars, sign, {(a:String, b:String) => Conditions.AndNotIf(a,b)},
                            {(a:String, b:String, c:Int) => Conditions.AndNotWhile(a,b,c)})

                        }
                      else if(Symbol.isAnd(chars) && Symbol.isAnd(sign))
                        {
                          handleSameOperators(signs, operands, chars, sign,
                            {(a:String, b:String, c:Int) => Conditions.AndAndWhile(a,b,c)})


                        }
                      else if(Symbol.isOr(chars) && !Symbol.isOr(sign)) // ---------- OR -------------
                      {
                        handleDifferentOperators(signs, operands, chars, sign, {(a:String, b:String) => Conditions.OrNotIf(a,b)},
                          {(a:String, b:String, c:Int) => Conditions.OrNotWhile(a,b,c)})

                      }
                      else if(Symbol.isOr(chars) && Symbol.isOr(sign))
                      {
                        handleSameOperators(signs, operands, chars, sign,
                          {(a:String, b:String, c:Int) => Conditions.OrOrWhile(a,b,c)})


                      }
                      else if(Symbol.isImplication(chars) && !Symbol.isImplication(sign)) // ---------- IMPLICATION -------------
                      {
                        handleDifferentOperators(signs, operands, chars, sign, {(a:String, b:String) => Conditions.ImplicationNotIf(a,b)},
                          {(a:String, b:String, c:Int) => Conditions.ImplicationNotWhile(a,b,c)})

                      }
                      else if(Symbol.isImplication(chars) && Symbol.isImplication(sign))
                      {
                        handleSameOperators(signs, operands, chars, sign,
                          {(a:String, b:String, c:Int) => Conditions.ImplicationImplicationWhile(a,b,c)})


                      }
                      else if(Symbol.isLeftImplication(chars)) // ---------- LEFT IMPLICATION -------------
                      {
                        handleSameOperators(signs, operands, chars, sign,
                          {(a:String, b:String, c:Int) => Conditions.LeftImplicationWhile(a,b,c)})

                      }
                      else if(Symbol.isLeftClosure(chars))
                      {
                        while(!Symbol.isRightClosure(sign))
                          {
                            operands.push(signs.pop());
                            sign = signs.top.toString;
                          }

                      }

                    }
                }
            }

        }
        result = connectStacks(signs, operands)

      }
      result;
  }


  def processOperator(chars: String, operands: mutable.Stack[String]) = {
    if(!Symbol.isNot(chars))
    {
      var num1 = true
      var num2 = true
      var booleanResult : Boolean = false;
      var x = Integer.parseInt(operands.pop.toString)
      var y = Integer.parseInt(operands.pop.toString)

      if(y == 0) num1 = false;
      if(x == 0) num2 = false;
      var result = 1;
      if(Symbol.isAnd(chars)) booleanResult = num1 & num2;
      else if(Symbol.isOr(chars)) booleanResult = num1 | num2;
      else if(Symbol.isImplication(chars)) booleanResult = !(num1) | num2;
      else booleanResult = (num1 & num2) | ((!num1) & !(num2));

      if(!booleanResult) result = 0;
      operands.push(result.toString);

    }
    else
    {
      var x = Integer.parseInt(operands.pop.toString);
      if(x==1) x = 0;
      else x = 1;
      operands.push(x.toString);
    }
  }

  override def evaluate(expression: String): Boolean =
    {
      var evaluation: Boolean = false;
      if(expression == null || expression.isEmpty)
        {
          throw null;
        }
      else {
        {
            var operands = new mutable.Stack[String]
            var splited = expression.split("")
            for(i<-splited.indices)
            {
                var chars = splited(i);
                if(chars.equals(" ")) {} // continue
                else
                  {
                    if(!Symbol.isOperator(chars)) operands.push(chars)
                    else
                      {
                        if(operands.size() == 1 && !Symbol.isNot(chars)) throw null
                        else processOperator(chars, operands)
                      }
                  }
            }
            if(operands.pop.toString.equals("1"))
            {
              evaluation = true;
            }
        }
        evaluation;
      }
    }
}

