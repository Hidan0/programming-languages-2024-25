import scala.util.parsing.combinator._

class ArithmeticParserCombinators extends JavaTokenParsers {
  def numberWithOp = wholeNumber ~ ("+"|"-"|"=") ^^ {case num ~ op => (num.toInt,op)}
  def dashedLine = "-+".r
  def script = rep(numberWithOp) ~ dashedLine ~ wholeNumber ^^ {
    case (lines ~ dash ~ result) => (lines, result.toInt)
    case _ => throw new IllegalArgumentException("Invalid input")
  }

  private def eval(expr: List[(Int, String)]): Int = {
    expr.foldLeft((0, "+")) {
      case ((acc, "+"), (num, "=")) => (acc + num, "_")
      case ((acc, "-"), (num, "=")) => (acc - num, "_")
      case ((acc, "+"), (num, nextOp)) => (acc + num, nextOp)
      case ((acc, "-"), (num, nextOp)) => (acc - num, nextOp)
      case _ => throw new IllegalArgumentException("Invalid expression")
    }._1
  }

  def parseAndValidate(input: String): Either[String, Boolean] = {
    parseAll(script, input) match {
      case Success((numberWithOperators, result), _) => {
        val res = eval(numberWithOperators)
        if (res == result)
          return Right(true)
        else
          return Left(s"Computed result ($res) does not match provided result ($result)")
      }
      case Failure(msg, _) => Left(s"Parsing failed: $msg")
      case Error(msg, _)   => Left(s"Parsing error: $msg")
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val p = new ArithmeticParserCombinators
    try {
      val script = scala.io.Source.fromFile(args(0)).mkString
      script
        .split("\n\n")
        .filter(_.trim.nonEmpty)
        .foreach(s => {
          p.parseAndValidate(s) match {
            case Right(_) => println(s"The script: \n\n$s\n\nis valid and the result is correct.")
            case Left(err) => println(s"Validation error for: \n\n$s\n\nError: $err")
          }
          println()
        })
    } catch {
      case ex: Exception =>
        println(s"Failed to read the file: ${ex.getMessage}")
    }
  }
} 
