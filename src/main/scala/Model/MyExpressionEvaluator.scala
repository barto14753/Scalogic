package Model

import Interfaces.ExpressionEvaluator
import Objects.Symbol
import javax.management.RuntimeErrorException

import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.collection.convert.ImplicitConversions.`mutableSeq AsJavaList`
import scala.collection.mutable
import scala.collection.mutable.Stack


class MyExpressionEvaluator extends ExpressionEvaluator
{
  override def getTruthValue(expression: String) =
  {
    println("Expression: " + expression)
    evaluate(infixToPostfix(expression));
  }

  override def infixToPostfix(expression: String): String =
  {
    var result: String = "";
    if(expression.isEmpty()) throw null;
    else
    {
      println("Checking if there are two operators next to each other")
      for(i <- 1 until expression.length())
      {
        val prev = expression.charAt(i - 1).toString;
        val curr = expression.charAt(i).toString;
        println("I = " + i + " | prev: " + prev + " | curr: " + curr)

        if(Symbol.isOperator(prev) && Symbol.isOperator(curr))
          {
            if(!Symbol.isNot(curr)) throw new RuntimeErrorException(null);
          }
      }
        var signs = new mutable.Stack[String];
        var operands = new mutable.Stack[String];

      for(i<- 0 until expression.length())
        {
          var chars = expression.charAt(i).toString;
          println("\n\nI: " + i)
          println("Char: " + chars)
          println("Before:")
          println("Signs: " + signs)
          println("Operands: " + operands)

          if(!Symbol.isOperator(chars))
            {
              if(Symbol.isLeftClosure(chars))
                {
                  println("Got left closure")
                  signs.push(chars);
                }
              else if(Symbol.isRightClosure(chars))
                {
                  println("Got right closure -> closing it")
                  while(!Symbol.isLeftClosure(signs.top.toString)) {
                    {
                      operands.push(signs.pop());
                    }
                    signs.pop();
                  }
                }
              else
                {
                  println("Got variable")
                  operands.push(chars);
                }
            }
          else
            {
              if(signs.isEmpty || Symbol.isLeftClosure(signs.top.toString))
                {
                  println("Signs is empty or on top of signs is LeftClosure")
                  if(!signs.isEmpty() && signs.top.equals(chars))
                    {
                      throw null
                    }
                    signs.push(chars);

                }
              else
                {
                  var sign = signs.top.toString;
                  println("Sign: " + sign)
                  if(chars == " ")
                    {
                      // continue
                    }
                  else
                    {
                      if(Symbol.isNot(chars))
                        {
                          println("Got NOT")
                          signs.push(chars);
                        }
                      else if(Symbol.isAnd(chars) && !Symbol.isAnd(sign)) // ---------- AND -------------
                        {
                          println("Char is AND and sign is not AND")
                          if(Symbol.isOr(sign) || Symbol.isImplication(sign) || Symbol.isLeftImplication(sign))
                            {
                              signs.push(chars);
                            }
                          else
                            {
                              var flag = 0;
                              do
                                {
                                  operands.push(signs.pop());
                                  if(signs.nonEmpty)
                                    {
                                      sign = signs.pop.toString;
                                    }
                                  else
                                    {
                                    flag += 1;
                                    }
                                }while(Symbol.isAnd(chars) && Symbol.isNot(sign) && flag ==0);
                              signs.push(chars);
                              flag = 0;
                            }
                        }
                      else if(Symbol.isAnd(chars) && Symbol.isAnd(sign))
                        {
                          println("Char is AND and sign is also AND")
                          var flag = 0;
                          do
                            {
                              operands.push(signs.pop());
                              if(signs.nonEmpty)
                                {
                                  sign = signs.pop.toString;
                                }
                              else
                                {
                                  flag += 1;
                                }
                            }while(Symbol.isAnd(chars) && Symbol.isAnd(sign) && flag == 0);
                          signs.push(chars);
                          flag = 0;

                        }
                      else if(Symbol.isOr(chars) && !Symbol.isOr(sign)) // ---------- OR -------------
                      {
                        println("Char is OR and sign is not OR")
                        if(Symbol.isImplication(sign) || Symbol.isLeftImplication(sign))
                        {
                          signs.push(chars);
                        }
                        else
                        {
                          var flag = 0;
                          do
                          {
                            operands.push(signs.pop)
                            if(signs.nonEmpty)
                            {
                              sign = signs.pop.toString;
                            }
                            else
                            {
                              flag += 1;
                            }
                          }while(Symbol.isOr(chars) && (Symbol.isAnd(sign) || Symbol.isNot(sign)) && flag ==0);
                          signs.push(chars);
                          flag = 0;
                        }
                      }
                      else if(Symbol.isOr(chars) && Symbol.isOr(sign))
                      {
                        println("Char is OR and sign is also OR")
                        println(signs)
                        var flag = 0;
                        do
                        {
                          operands.push(signs.pop)
                          if(signs.nonEmpty)
                          {
                            sign = signs.pop.toString;
                          }
                          else
                          {
                            flag += 1;
                          }
                        }while(Symbol.isOr(chars) && Symbol.isOr(sign) && flag == 0);
                        signs.push(chars);
                        flag = 0;

                      }
                      else if(Symbol.isImplication(chars) && !Symbol.isImplication(sign)) // ---------- IMPLICATION -------------
                      {
                        if(Symbol.isLeftImplication(sign))
                        {
                          signs.push(chars);
                        }
                        else
                        {
                          var flag = 0;
                          do
                          {
                            operands.push(signs.pop());
                            if(signs.nonEmpty)
                            {
                              sign = signs.pop.toString;
                            }
                            else
                            {
                              flag += 1;
                            }
                          }while(Symbol.isImplication(chars) && (Symbol.isAnd(sign) || Symbol.isNot(sign) || Symbol.isOr(sign)) && flag ==0);
                          signs.push(chars);
                          flag = 0;
                        }
                      }
                      else if(Symbol.isImplication(chars) && Symbol.isImplication(sign))
                      {
                        var flag = 0;
                        do
                        {
                          operands.push(signs.pop());
                          if(signs.nonEmpty)
                          {
                            sign = signs.pop.toString;
                          }
                          else
                          {
                            flag += 1;
                          }
                        }while(Symbol.isImplication(chars) && Symbol.isImplication(sign) && flag == 0);
                        signs.push(chars);
                        flag = 0;

                      }
                      else if(Symbol.isLeftImplication(chars)) // ---------- LEFT IMPLICATION -------------
                      {
                        var flag = 0;
                        do
                          {
                            operands.push(signs.pop());
                            if(signs.nonEmpty)
                            {
                              sign = signs.pop.toString;
                            }
                            else
                            {
                              flag += 1;
                            }
                          }while(Symbol.isImplication(chars) && (Symbol.isAnd(sign) || Symbol.isNot(sign) || Symbol.isOr(sign)) && flag ==0);
                        signs.push(chars);
                        flag = 0;
                      }
                      else if(Symbol.isLeftClosure(chars))
                      {
                        while(!Symbol.isRightClosure(sign))
                          {
                            operands.push(signs.pop());
                            sign = signs.pop.toString;
                          }

                      }

                    }
                }
            }
          println("\nAfter:")
          println("Signs: " + signs)
          println("Operands: " + operands)
        }

        var counter = signs.size();
        println("\n\nNext")
        for(i<-1 until counter+1)
        {
          if(Symbol.isClosure(signs.top.toString))
          {
            throw null
          }
          println("Push " + signs.top + " to operands")
          operands.push(signs.pop());
        }
        var result1 = new Array[String](operands.length);
        println("Signs: " + signs)
        println("Operands: " + operands)
        println(operands.size + " = " + expression.length)

        for(i <- operands.indices)
        {
          println(i + ": " + operands.top)
          result1(i) = operands.pop()
        }
        var res = ""
        for(i<-result1.length-1 to 0 by -1)
        {
          println(i + ": " + result1(i))
          if(i>0) res ++ " ";
          res = res ++ result1(i);
        }
        result = res
        println("Result: " + res)

      }
      result;
  }


  override def evaluate(expression: String): Boolean =
    {
      println("\nEvaluate: " + expression)
      var evaluation: Boolean = false;
      if(expression == null || expression.isEmpty)
        {
          throw null;
        }
      else {
        {
          for(i<-1 until expression.length()) {
            {
              var curr = expression.charAt(i).toString;
              var prev = expression.charAt(i-1).toString;
              if(Symbol.isOperator(curr) && Symbol.isOperator(prev))
                {
                  if(Symbol.isNot(curr))
                    {
                      throw new RuntimeErrorException(null);
                    }
                }
            }
            var operands = new mutable.Stack[String]
            var splited = expression.split("")
            println("Splited expression: " + splited.mkString("Array(", ", ", ")"))
            for(i<-splited.indices)
            {
                println("\n\nProcess " + i + ": " + splited(i))
                println("Operands: " + operands)
                var chars = splited(i);
                if(chars.equals(" "))
                  {
                    // TODO continue
                  }
                else
                  {
                    if(!Symbol.isOperator(chars))
                      {
                        println("Got variable")
                        operands.push(chars);
                        println("Operands: " + operands)
                      }
                    else
                      {
                        if(operands.size() == 1 && !Symbol.isNot(chars))
                          {
                            throw null;
                          }
                        else
                          {
                            if(!Symbol.isNot(chars))
                              {
                                println("Operator is not NOT")
                                println("Operands: " + operands)
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
                      }
                  }


            }
            if(operands.pop.toString.equals("1"))
            {
              evaluation = true;
            }
          }

        }
        evaluation;
      }
    }
}

