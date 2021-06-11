package Model

import Interfaces.ExpressionEvaluator

class TruthTableHandler(rows: Int, cols: Int, symbols: Array[Char]) {
  var vars: Int = cols - 1
  var truthTable: Array[Array[Boolean]] = Array.ofDim[Boolean](rows, cols)
  var expressionEvaluator = new MyExpressionEvaluator()

  def toBinary(n:Int, bin: List[Int] = List.empty[Int]): String = {
    if(n / 2 == 1) (1:: (n % 2) :: bin).mkString(" ")
    else {
      val r = n % 2
      val q = n / 2
      toBinary(q, r::bin)
    }
  }

  def generateTruthTable(expression: String): Array[Array[Boolean]] = {
    println("Generate Truth Table | expression: " + expression)
    println("Rows: " + rows)
    for(subset <- 0 until rows){
      //("Subset: " + subset)
      //var subsetBits: String = toBinary(subset)
      var subsetBits: String = subset.toBinaryString
      var substitutedExpression: String = expression

      while(subsetBits.length() < vars){
        subsetBits = "0" + subsetBits
      }

      for(i <- 0 until subsetBits.length()){
        truthTable(subset)(i) = (subsetBits.charAt(i) == '1')
      }
      var i = 0
      for(symbol <- symbols){
        println("SubsetBits symbol: " + symbol + " | i = " + i)

        substitutedExpression = substitutedExpression.replaceAll(symbol + "", subsetBits.charAt(i) + "")
        i = i + 1
      }
      val truthValue = { expressionEvaluator.getTruthValue(substitutedExpression) }
      truthTable(subset)(cols - 1) = truthValue
    }
    truthTable
  }

  def testEquivalence(truthTable: Array[Array[Boolean]], comparedTruthtable: Array[Array[Boolean]]): Boolean = {
    if(truthTable.length != comparedTruthtable.length){
      return false
    }

    for(i <- truthTable.indices){
      if(!truthTable(i)(truthTable(0).length - 1).equals(comparedTruthtable(i)(comparedTruthtable(0).length - 1))){
        false
      }
    }
    true
  }
}
