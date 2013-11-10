package calculator

import scala.util.parsing.combinator.RegexParsers

import Calculator._

class Calculator {

  private class EvaluationException(val msg: String) extends Exception(msg)

  def evaluate(expr: String): EvalResult = {
    try {
      parser.parseAll(parser.setVarExpr, expr).getOrElse(Left("invalid expression"))
    } catch {
      case e: EvaluationException => Left(e.msg)
    }
  }

  private val parser = new RegexParsers {

    var vars: Map[String, Double] = Map()
    val constants = Map(("pi" -> Math.PI), "e" -> Math.E)

    def setVarExpr: Parser[EvalResult] = opt(varName <~ "=") ~ expr ^^ {
      case Some(name) ~ n => {
        assignValue(name, n)
        Right(EvalInfo(Some(name), n))
      }
      case parExpr ~ n => Right(EvalInfo(None, n))
    }
    def expr: Parser[Double] = opt("-" | "+") ~ sums ^^ {
      case Some("-") ~ n => -n
      case parExpr ~ n => n
    }
    def sums = ternaryOpsParser(divMult, ("-" | "+"))
    def divMult = ternaryOpsParser(pow, ("*" | "/"))
    def pow = ternaryOpsParser(parExpr, "^")
    def parExpr = value | "(" ~> expr <~ ")"
    def value = functionApplication | num | variable
    def functionApplication = functionName ~ ("(" ~> expr) <~ ")" ^^ {
      case fName ~ n => applyFunction(fName, n)
    }
    def variable = varName ^^ { valueByName(_) }
    def num = number ^^ { _.toDouble }

    val number = """[0-9]+(\.[0-9]+)?""".r
    val varName = "[a-zA-Z]+[0-9]*".r
    val functionName = "[a-zA-Z]+".r

    val ternaryOps: Map[String, (Double, Double) => Double] = Map(
      "+" -> { _ + _ },
      "-" -> { _ - _ },
      "*" -> { _ * _ },
      "/" -> { _ / _ },
      "^" -> { Math.pow(_, _) })

    def ternaryOpsParser(nextParser: Parser[Double], op: Parser[String]): Parser[Double] = {
      nextParser ~ rep(op ~ nextParser) ^^ {
        case n ~ r => (n /: r) {
          case (z, op ~ next) => ternaryOps(op)(z, next)
        }
      }
    }

    val unaryFunctions: Map[String, Double => Double] = Map(
      "cos" -> { x => Math.cos(x) },
      "sin" -> { x => Math.sin(x) },
      "tg" -> { x => Math.tan(x) },
      "ctg" -> { x => Math.tan(x) })

    def applyFunction(name: String, value: Double) = {
      unaryFunctions.get(name) match {
        case Some(f) => f(value)
        case None => throw new EvaluationException(format("'%s' is not a valid function name", name))
      }
    }

    def valueByName(n: String): Double = {
      vars.getOrElse(n, constants.getOrElse(n,
        throw new EvaluationException(format("'%s' is not a valid variable name", n))))
    }

    def assignValue(name: String, value: Double) = {
      if (constants.contains(name)) {
        throw new EvaluationException("Can't change value of a constant")
      }
      vars += (name -> value)
    }
  }

}

object Calculator {

  type EvalResult = Either[String, EvalInfo]
  case class EvalInfo(varName: Option[String], result: Double) {
    override def toString = varName match {
      case None => formattedResult
      case Some(name) => format("%s = %s", name, formattedResult)
    }

    def formattedResult = result.toString
  }

  def help() = println("""
  This an interactive calculator illustrating the usage of scala parser combinators 
  
  by Mikhail Golubtsov, 2013
    
  MANUAL
  
  Enter an arithmetic expression and get the result immediately.
  Allowed operators: unary & ternary '-', '+', '*', '/', '^'.
  
  You could use constants 'pi' and 'e'.
  
  There are predefined functions - 'sin', 'cos', 'tg', 'ctg'.
  For example,
  
  > sin(0)
  0
  
  Also you could assign the result to a variable and use it in any later expressions.
  For example, 
  
  > x = 2 * 5
  x = 10
  
  > x * 2
  20 
  
  To see this manual again enter "help"
    
  To terminate enter "quit"
    
  """)

  def main(args: Array[String]): Unit = {
    help()
    val calc = new Calculator()
    while (true) {
      val nextExpr = readLine() match {
        case "quit" => return
        case "help" => help()
        case expr => {
          val r = calc.evaluate(expr)
          val output = formatResult(r)
          println(output)
          println
        }
      }
    }
  }

  def formatResult(r: EvalResult): String = r match {
    case Left(errMsg) => "ERROR: " + errMsg
    case Right(info) => info.toString
  }

}