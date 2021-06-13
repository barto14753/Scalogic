import Model.MyExpressionEvaluator
import org.scalatest.FunSuite

class EvaluateTest extends FunSuite{
  val evaluator = new MyExpressionEvaluator

  test("Evaluate: 00v"){
    assert(!evaluator.evaluate("00v"))
  }

  test("Evaluate: 01v"){
    assert(evaluator.evaluate("01v"))
  }

  test("Evaluate: 11v"){
    assert(evaluator.evaluate("11v"))
  }

  test("Evaluate: 00v0^"){
    assert(!evaluator.evaluate("00v0^"))
  }

  test("Evaluate: 10v1^"){
    assert(evaluator.evaluate("10v1^"))
  }

  test("Evaluate: 11v1^"){
    assert(evaluator.evaluate("11v1^"))
  }
  //
  test("Evaluate: 0~0v0>"){
    assert(!evaluator.evaluate("0~0v0>"))
  }

  test("Evaluate: 1~0v1>"){
    assert(evaluator.evaluate("1~0v1>"))
  }

  test("Evaluate: 1~1v1>"){
    assert(evaluator.evaluate("1~1v1"))
  }
}
