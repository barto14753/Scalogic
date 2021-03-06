package Model

import Interfaces.ExpressionEvaluator

class TruthTableHandler(rows: Int, cols: Int, symbols: Array[Char]) {
  var vars: Int = cols - 1
  var truthTable: Array[Array[Boolean]] = Array.ofDim[Boolean](rows, cols)
  var expressionEvaluator = new MyExpressionEvaluator()


  def generateTruthTable(expression: String): Array[Array[Boolean]] = {
    for(subset <- 0 until rows){

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
