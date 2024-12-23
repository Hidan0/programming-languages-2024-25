import scala.util.parsing.combinator._

sealed trait Expr 
case class Num(value: Int) extends Expr
case class BinOp(left: Expr, op: String, right: Expr) extends Expr 

object StepByStepPC extends JavaTokenParsers {
  def number = wholeNumber ^^ {n => Num(n.toInt)}
  def op = "+"|"-"|"*"|"/"
  def expr: Parser[Expr] = number | "(" ~> expr ~ op ~ expr <~ ")" ^^ {case l~operator~r => BinOp(l, operator, r)}

  private def serialize(ast: Expr): String = {
    ast match {
      case Num(value) => value.toString
      case BinOp(l, o, r) => s"(${serialize(l)} $o ${serialize(r)})"
    }
  }

  private def isFullyReduced(expr: Expr): Boolean = {
    expr match {
      case Num(_) => true
      case _ => false
    }
  }

  private def reduceInnerMost(expr: Expr): Expr = {
    expr match {
      case BinOp(Num(l), o, Num(r)) => {
        val res = o match {
          case "*" => l * r
          case "/" => l / r
          case "+" => l + r
          case "-" => l - r
        }
        Num(res)
      }
      case BinOp(l, o, r) => {
        val sx = if (isFullyReduced(l)) l else reduceInnerMost(l) 
        val dx = if (isFullyReduced(r)) r else reduceInnerMost(r) 
        BinOp(sx, o, dx)
      }
      case e => throw new IllegalArgumentException(f"Unexpected expression structure: $e")
    }
  }

  private def evaluateWithSteps(ast: Expr): Int = {
    println(serialize(ast))
    ast match {
      case Num(value) => value
      case BinOp(l, o, r) => {
        evaluateWithSteps(reduceInnerMost(BinOp(l, o, r)))
      }
    }
  }

  def stepByStepEval(input: String): Int = {
    parseAll(expr, input) match {
      case Success(ast, _) => {
        evaluateWithSteps(ast)
      }
      case x => throw new IllegalArgumentException(f"Error: ${x.toString}")
    }
  }
} 

object Main {
  def main(args: Array[String]): Unit = {
    args.filter(_.trim.nonEmpty).foreach {ex => {
      StepByStepPC.stepByStepEval(ex)
      println("---")
    }}
  }
}
