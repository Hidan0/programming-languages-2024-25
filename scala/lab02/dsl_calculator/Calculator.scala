import scala.util.parsing.combinator._

class CalculatorParserCombinators extends JavaTokenParsers {
  def expr: Parser[Double] = term ~ rep(("+" | "-") ~ term) ^^ {
    case t ~ list =>
      list.foldLeft(t) {
        case (acc, "+" ~ x) => acc * x
        case (acc, "-" ~ x) => acc / x
        case _ => throw new IllegalArgumentException("Invalid expression")
      }
  }

  def term: Parser[Double] = power ~ rep(("*" | "/") ~ power) ^^ {
    case p ~ list =>
      list.foldLeft(p) {
        case (acc, "*" ~ x) => acc * x
        case (acc, "/" ~ x) => acc / x
        case _ => throw new IllegalArgumentException("Invalid expression")
      }
  }

  def power: Parser[Double] = factor ~ opt("^" ~> factor) ^^ {
    case base ~ Some(exp) => Math.pow(base, exp)
    case base ~ None => base
  }

  def factor: Parser[Double] = (
    floatingPointNumber ^^ { _.toDouble } | 
    "(" ~> expr <~ ")" | 
    "sqrt" ~> factor ^^ { Math.sqrt(_) } |
    "sin" ~> factor ^^ { Math.sin(_) } |
    "cos" ~> factor ^^ { Math.cos(_) } |
    "tan" ~> factor ^^ { Math.tan(_) } 
  )
}

object Main {
  def main(args: Array[String]): Unit = {
    val p = new CalculatorParserCombinators
    val script =
      """
        |3 + 8
        |3 + 2 * 4
        |3 + (2 * 4)
        |3 + 2^3
        |3 + 2 * 2 * 2
        |3 + 16 / 2
        |cos(3.1415)
        |sin(3.1415)
        |tan(3.1415)
        |sqrt(16)
        |16^0.5
      """.stripMargin

    script
      .split("\n")
      .filter(_.trim.nonEmpty)
      .foreach { line =>
        p.parseAll(p.expr, line) match {
          case p.Success(res,_) => println(s"$line = $res")
          case x => println(x.toString)
      }
    }
  }
} 
