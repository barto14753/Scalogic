package Model
import Objects.Symbol
import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = {



    var expressionHandler = new ExpressionHandler("p v q v r")
    expressionHandler.showTruthTable()



  }
}
