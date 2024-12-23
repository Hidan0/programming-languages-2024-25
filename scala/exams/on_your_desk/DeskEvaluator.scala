import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap

class DeskParserCombinator extends JavaTokenParsers {
  def interger: Parser[Either[Int,String]] = wholeNumber ^^ {n => Left(n.toInt)}
  def variable: Parser[Either[Int,String]] = """[a-zA-Z]+""".r ^^ {i => Right(i)}

  def term = interger | variable 

  def expr: Parser[List[Either[Int,String]]] = repsep(term, "+")

  def vars = repsep("""[a-zA-Z]+""".r ~ "=" ~ wholeNumber, ",") ^^ {
    case (terms) => 
      var env: HashMap[String, Int] = HashMap()
      terms.foreach {
        case name ~ "=" ~ value => env(name) = value.toInt
        case _ => throw new IllegalArgumentException
      }
      env
  } 

  def program = phrase("print" ~> expr ~ ("where" ~> vars)) ^^ {
    case terms ~ env =>
      (terms
        .map { term =>
          term match {
            case Left(value) => value
            case Right(name) => env(name)
          }
        }
        .sum, env)
    case _ => throw new IllegalArgumentException
  }
}

object DeskEvaluator {
  def main(args: Array[String]): Unit = {
    try {
      val input = scala.io.Source.fromFile(args(0)).mkString
      val p = new DeskParserCombinator
      input
        .split('\n')
        .filter(_.trim.nonEmpty)
        .foreach { line =>
          p.parseAll(p.program, line) match {
            case p.Success((result,env), _) => {
              println(result)
              println(env.toString)
              println("---")
            }
            case x => println(s"Failed to parse: ${x.toString}")
          }
        }
    } catch {
      case ex: Exception => 
        println(s"Failed to read the file: ${ex.getMessage}")
    }
  }
}
