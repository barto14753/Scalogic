package Model

import scala.collection.mutable

class ExpressionHandler {
  var truthTable: Array[Array[Boolean]]  = Array.ofDim[Boolean](0, 0)
  var expression: String = " "
  var cols: Int = calcCols()
  var rows: Int = calcRows()
  var symbols: Array[Char] = initSymbols()
  var truthTableHandler = new TruthTableHandler(rows, cols, symbols)
  var numberOfVariables: Int = initNumberOfVariables()
  var tautology = false
  var contradiction = false
  val freq = new mutable.HashMap[Char, Int]()


  def getFreqMap: mutable.HashMap[Char, Int] = {

    for(c <- expression.toCharArray){
      if(Character.isAlphabetic(c) && !c.equals('v') && !c.equals('V')) {
        if(freq.isDefinedAt(c)){
          var t = 0
          for(i <- freq.get(c)) { t = i }
          t += 1
          freq.put(c, t)
        }
      } else {
        freq.put(c, 1)
      }
    }
    freq
  }

  def initSymbols(): Array[Char] ={
    freq.keySet.toArray
  }

  def calcRows(): Int = {
    var pow2: Int = 1
    for(_ <- 0 until numberOfVariables) {
      pow2 *= 2
    }
    pow2
  }

  def calcCols(): Int = {
    numberOfVariables + 1
  }

  def initNumberOfVariables(): Int = {
    freq.size
  }

  def  getExpression: String = {
    expression
  }

  def setExpression(expression: String): Unit = {
    this.expression = expression
  }

  def initTruthTable(): Unit = {
    truthTableHandler.generateTruthTable(this.expression)
  }

  def getTruthTable: Array[Array[Boolean]] = {
    if(truthTable.length == 0){
      initTruthTable()
    }
    truthTable
  }

  def getSymbols: Array[Char] = {
    symbols
  }

  def getNumberOfVariables: Int = {
    numberOfVariables
  }

  def testTautology(): Unit = {
    for(row <- truthTable; i <- row.indices) {
        if((row(i)).eq(false)) {
          tautology = false
          return
      }
    }
    tautology = true
  }

  def testContradiction(): Unit = {
    for(row <- truthTable; i <- row.indices) {
      if((row(i)).eq(true)) {
        tautology = false
        return
      }
    }
    tautology = true
  }

  def isContradiction() = {
    if(contradiction == null) {
      testContradiction()
    }
    contradiction
  }

  def isEquivalence(comparedTrutTable: Array[Array[Boolean]]): Boolean = {
    truthTableHandler.testEquivalence(truthTable, comparedTrutTable)
  }



}
