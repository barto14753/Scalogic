package Objects

import scala.collection.mutable.Set

object Symbol
{
  private var and = "^";
  private var or = "v";
  private var not = "~";
  private var implication = ">";
  private var leftClosure = "(";
  private var rightClosure = ")";

  private var Operators = Set(and, or, not, implication);
  private var Closures = Set(leftClosure, rightClosure);
  private var Symbols = Operators ++ Closures;

  def isSymbol(candidate: String) = Symbols.contains(candidate);
  def isClosure(candidate: String) = Closures.contains(candidate);
  def isOperator(candidate: String) = Operators.contains(candidate);

  def isAnd(candidate: String) = candidate == and;
  def isOr(candidate: String) = candidate == or;
  def isNot(candidate: String) = candidate == not;
  def isLeftClosure(candidate: String) = candidate == leftClosure;
  def isRightClosure(candidate: String) = candidate == rightClosure;

  def getOperators = Operators;
  def getClosures = Closures;
  def getSymbols = Symbols;

}
