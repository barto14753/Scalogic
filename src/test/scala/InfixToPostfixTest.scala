import Model.MyExpressionEvaluator
import org.scalatest.FunSuite

class InfixToPostfixTest extends FunSuite {
  val evaluator = new MyExpressionEvaluator
  test("InfixToPostfix: pvq = 0v0"){
    assert(evaluator.infixToPostfix("0v0") === "00v")
  }

  test("InfixToPostfix: pvq = 0v1"){
    assert(evaluator.infixToPostfix("0v1") === "01v")
  }

  test("InfixToPostfix: pvq = 1v1"){
    assert(evaluator.infixToPostfix("1v1") === "11v")
  }

  test("InfixToPostfix: (pvq)^r = (0v0)^0"){
    assert(evaluator.infixToPostfix("(0v0)^0") === "00v0^")
  }

  test("InfixToPostfix: (pvq)^r = (1v0)^1"){
    assert(evaluator.infixToPostfix("(1v0)^1") === "10v1^")
  }

  test("InfixToPostfix: (pvq)^r = (1v1)^1"){
    assert(evaluator.infixToPostfix("(1v1)^1") === "11v1^")
  }
//
  test("InfixToPostfix: (~pvq)>r = (~0v0)>0"){
    assert(evaluator.infixToPostfix("(~0v0)>0") === "0~0v0>")
  }

  test("InfixToPostfix: (~pvq)>r = (~1v0)>1"){
    assert(evaluator.infixToPostfix("(~1v0)>1") === "1~0v1>")
  }

  test("InfixToPostfix: (~pvq)>r = (~1v1)>1"){
    assert(evaluator.infixToPostfix("(~1v1)>1") === "1~1v1>")
  }



}
