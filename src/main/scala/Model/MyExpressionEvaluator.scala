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
    evaluate(infixToPostfix(expression));
  }

  override def infixToPostfix(expression: String): String =
  {
    var result: String = "";
    if(expression.isEmpty()) throw null;
    else
    {
      for(i <- 1 until expression.length()) {
      {
        val prev = expression.charAt(i - 1).toString;
        val curr = expression.charAt(i).toString;

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
          if(!Symbol.isOperator(chars))
            {
              if(Symbol.isLeftClosure(chars))
                {
                  signs.push(chars);
                };
              else if(Symbol.isRightClosure(chars))
                {
                  while(Symbol.isLeftClosure(signs.top().toString)) {
                    {
                      operands.push(signs.pop());
                    }
                    signs.pop();
                  }
                }
              else
                {
                  operands.push(chars);
                }
            }
          else
            {
              if(signs.isEmpty() || Symbol.isLeftClosure(signs.top().toString))
                {
                  if(!signs.isEmpty() && signs.top().equals(chars))
                    {
                      throw null
                    }
                    signs.push(chars);

                }
              else
                {
                  var sign = signs.top().toString;
                  if(chars == ' ')
                    {
                      // TODO
                    }
                  else
                    {
                      if(Symbol.isNot(chars))
                        {
                          signs.push(chars);
                        }
                      else if(Symbol.isAnd(chars) && !Symbol.isAnd(sign)) // ---------- AND -------------
                        {
                          if(Symbol.isAnd(sign) || Symbol.isImplication(sign) || Symbol.isLeftImplication(sign))
                            {
                              signs.push(chars);
                            }
                          else
                            {
                              var flag = 0;
                              do
                                {
                                  operands.push(signs.pop());
                                  if(!signs.isEmpty())
                                    {
                                      sign = signs.top().toString;
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
                          var flag = 0;
                          do
                            {
                              operands.push(signs.pop());
                              if(!signs.isEmpty())
                                {
                                  sign = signs.top().toString;
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
                        if(Symbol.isImplication(sign) || Symbol.isLeftImplication(sign))
                        {
                          signs.push(chars);
                        }
                        else
                        {
                          var flag = 0;
                          do
                          {
                            operands.push(signs.pop());
                            if(!signs.isEmpty())
                            {
                              sign = signs.top().toString;
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
                        var flag = 0;
                        do
                        {
                          operands.push(signs.pop());
                          if(!signs.isEmpty())
                          {
                            sign = signs.top().toString;
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
                            if(!signs.isEmpty())
                            {
                              sign = signs.top().toString;
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
                          if(!signs.isEmpty())
                          {
                            sign = signs.top().toString;
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
                            if(!signs.isEmpty())
                            {
                              sign = signs.top().toString;
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
                            sign = signs.top().toString;
                          }

                      }

                    }
                }
            }
        }

        var counter = signs.size();
        for(i<-1 until counter+1)
        {
          {
            if(Symbol.isClosure(signs.top().toString))
              {
                throw null
              }
              operands.push(signs.pop());
          }
          var result1 = new Array[String](expression.length());
          var l = operands.size()-1;
          for(i<- l to 0)
            {
              result1(i) = operands.pop();
            }
          var res = "";
          for(i<-0 until res.length()) {
            {
              if(i>0) res ++ " ";
              res = res ++ result1(i);
            }
            result = res;
          }

        }
      }
      result;
    }
  }

  override def evaluate(expression: String): Boolean =
    {
      var evaluation: Boolean = false;
      print(expression);
      if(expression == null || expression.isEmpty())
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
            var operands = new mutable.Stack[String];
            var splited = expression.split("");
            for(i<-0 until splited.length) {
              {
                var chars = splited(i);
                if(chars.equals(" "))
                  {
                    // TODO continue
                  }
                else
                  {
                    if(!Symbol.isOperator(chars))
                      {
                        operands.push(chars);
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
                                var num1 = true;
                                var num2 = true;
                                var booleanResult : Boolean = false;
                                var x = Integer.parseInt(operands.pop().toString());
                                var y = Integer.parseInt(operands.pop().toString());

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
                                var x=  Integer.parseInt(operands.pop().toString());
                                if(x==1) x = 0;
                                else x = 1;
                                operands.push(x.toString);
                              }
                          }
                      }
                  }
              }
              if(operands.pop().toString.equals("1"))
                {
                  evaluation = true;
                }
            }
          }

        }
        evaluation;
      }
    }
}

