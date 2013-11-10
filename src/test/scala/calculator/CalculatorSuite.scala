package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import calculator.Calculator._

@RunWith(classOf[JUnitRunner])
class CalculatorTest extends FunSuite {

  trait TestEnv {
    val calc = new Calculator()

    def evalExprTo(input: String, expected: Double) = {
      evalExpr(input, { info =>
        if (info.varName.isDefined) {
          fail("Variable name should not be defined for simple expression")
        }
        assertDoubles(expected, info.result)
      })
    }

    def setVarTo(input: String, expectedName: String, expectedVal: Double) = {
      evalExpr(input, { info =>
        val varName = info.varName.getOrElse(fail("There were no var name returned"))
        assert(expectedName === varName)
        assertDoubles(expectedVal, info.result)
      })
    }

    def isErrorOn(input: String) = assert(calc.evaluate(input).isLeft)

    private def evalExpr(input: String, onSuccess: EvalInfo => Unit) =
      calc.evaluate(input) match {
        case Left(errMsg) => fail("Fail to parse expression: " + errMsg)
        case Right(info) => onSuccess(info)
      }

    private def assertDoubles(expected: Double, actual: Double) =
      assert(Math.abs(actual - expected) < 0.00001D)
  }

  test("no input - error") {
    new TestEnv {
      assert(calc.evaluate("").isLeft)
    }
  }

  test("simple integer") {
    new TestEnv {
      evalExprTo("11", 11)
    }
  }

  test("simple float") {
    new TestEnv {
      evalExprTo("11.11", 11.11F)
    }
  }

  test("parenthesis") {
    new TestEnv {
      evalExprTo("(11)", 11)
    }
  }

  test("mismatched parenthesis") {
    new TestEnv {
      isErrorOn("1*(1+1")
    }
  }

  test("plus sign") {
    new TestEnv {
      evalExprTo("+11", 11)
    }
  }

  test("minus sign") {
    new TestEnv {
      evalExprTo("-11", -11)
    }
  }

  test("double minus sign") {
    new TestEnv {
      evalExprTo("-(-11)", 11)
    }
  }

  test("sum") {
    new TestEnv {
      evalExprTo("2+2", 4)
    }
  }

  test("subtract") {
    new TestEnv {
      evalExprTo("2-2", 0)
    }
  }

  test("sum & subtract equal precedence") {
    new TestEnv {
      evalExprTo("1+1-1+2", 3)
    }
  }

  test("multiplication") {
    new TestEnv {
      evalExprTo("2*2", 4)
    }
  }

  test("division") {
    new TestEnv {
      evalExprTo("2/4", 0.5F)
    }
  }

  test("division & multiplication equal precedence") {
    new TestEnv {
      evalExprTo("1*2/1*2", 4)
    }
  }

  test("multiplication has greater precedence than sum") {
    new TestEnv {
      evalExprTo("1+2*2", 5)
    }
  }

  test("pow") {
    new TestEnv {
      evalExprTo("2^3", 8)
    }
  }

  test("^ has higher precedence than *, /") {
    new TestEnv {
      evalExprTo("2*2^2", 8)
    }
  }

  test("set var") {
    new TestEnv {
      setVarTo("a = 1", "a", 1)
    }
  }

  test("var value used in expression") {
    new TestEnv {
      setVarTo("a = 1", "a", 1)
      evalExprTo("a * 3", 3)
    }
  }

  test("unknown var") {
    new TestEnv {
      isErrorOn("x * 3")
    }
  }

  test("pi constant") {
    new TestEnv {
      evalExprTo("pi", Math.PI.toFloat)
    }
  }

  test("exp constant") {
    new TestEnv {
      evalExprTo("e", Math.E.toFloat)
    }
  }

  test("failed to assign value to a constant") {
    new TestEnv {
      isErrorOn("pi = 4")
    }
  }

  test("invalid function name") {
    new TestEnv {
      isErrorOn("notFound(2*3)")
    }
  }

  test("cosinus") {
    new TestEnv {
      evalExprTo("cos(pi)", -1)
    }
  }

  test("sinus") {
    new TestEnv {
      evalExprTo("sin(pi/2)", 1)
    }
  }

  test("tangent") {
    new TestEnv {
      evalExprTo("tg(0)", 0)
    }
  }

  test("cotangent") {
    new TestEnv {
      evalExprTo("ctg(0)", 0)
    }
  }
}