package Model

import scala.collection.mutable

class ExpressionHandler(var expression: String) {
  var truthTable: Array[Array[Boolean]]  = Array.ofDim[Boolean](0, 0)
  var freq = new mutable.HashMap[Char, Int]()
  freq = getFreqMap
  var symbols: Array[Char] = initSymbols()
  var numberOfVariables: Int = initNumberOfVariables()
  var cols: Int = calcCols()
  var rows: Int = calcRows()
  var truthTableHandler = new TruthTableHandler(rows, cols, symbols)
  var tautology = false
  var contradiction = false



  def getFreqMap: mutable.HashMap[Char, Int] = {
    //("Loading signs")
    for(c <- expression.toCharArray){
      //println("Process: " + c)
      if(Character.isAlphabetic(c) && !c.equals('v') && !c.equals('V')) {
        if(freq.isDefinedAt(c)){
          //println("Is already defined")
          var t = 0
          for(i <- freq.get(c)) { t = i }
          t += 1
          freq.put(c, t)
        }
        else {
          //println("New sign")
          freq.put(c, 1)
        }
      }
    }
    //println(freq)
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
    truthTable = truthTableHandler.generateTruthTable(this.expression)
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
        if(!row(i)) {
          tautology = false
          return
      }
    }
    tautology = true
  }

  def testContradiction(): Unit = {
    for(row <- truthTable; i <- row.indices) {
      if(row(i)) {
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

  def showTruthTable(): Unit =
  {
    if(truthTable.length == 0){
      initTruthTable()
    }

    println("Expression: " + expression)
    var row = ""
    for(i <- symbols.indices){
      val symbol = symbols(i)
      val spaces = 5
      row = row + symbol + "     |"

    }
    row = row + "Result|"
    println(row)
    for(i <- truthTable.indices){
      row = ""
      for(j <- truthTable(i).indices){
        row = row + truthTable(i)(j)
        if(truthTable(i)(j)) row = row + "  |"
        else row = row + " |"

      }
      println(row)
    }
  }



}
