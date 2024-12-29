import scala.util.parsing.combinator._
import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

class ArnoldCEvaluator extends JavaTokenParsers {
  private var stack: Stack[Int] = Stack.empty
  private var variables: HashMap[String, Int] = HashMap.empty

  def svalue = stringLiteral ^^ {s => s.substring(1, s.length-1)}
  def ivalue: Parser[Int] = wholeNumber ^^ {_.toInt}
  def variable: Parser[Int] = ident ^^ {variables(_)}
  def value: Parser[Int] = ivalue|variable

  def declare_var = ("HEY" ~> "CHRISTMAS" ~> "TREE" ~> ident) ~ ("YOU" ~> "SET" ~> "US" ~> "UP" ~> ivalue) ^^ {case (name ~ value) =>
    variables(name) = value
  }

  def arithmetic = (
    ("GET" ~> "UP" ~> value) ^^ {v => stack.push(stack.pop() + v)} |
    ("GET" ~> "DOWN" ~> value) ^^ {v => stack.push(stack.pop() - v)} |
    ("YOU'RE" ~> "FIRED" ~> value) ^^ {v => stack.push(stack.pop() * v)} |
    ("HE" ~> "HAD" ~> "TO" ~> "SPLIT" ~> value) ^^ {v => stack.push(stack.pop() / v)} 
  )

  def logical = (
    ("YOU" ~> "ARE" ~> "NOT" ~> "YOU" ~> "ARE" ~> "ME" ~> value) ^^ {v => stack.push(if (stack.pop() == v) 1 else 0)} |
    ("LET" ~> "OFF" ~> "SOME" ~> "STEAM" ~> "BENNET" ~> value) ^^   {v => stack.push(if (stack.pop() > v) 1 else 0)}  |
    ("CONSIDER" ~> "THAT" ~> "A" ~> "DIVORCE" ~> value) ^^          {v => stack.push(stack.pop() * v)}                |
    ("KNOCK" ~> "KNOCK" ~> value) ^^                                {v => stack.push(stack.pop() + v)} 
  )

  def first_op = "HERE" ~> "IS" ~> "MY" ~> "INVITATION" ~> value ^^ {stack.push(_)}
  def operation = arithmetic|logical
  def operations = operation ~ rep(operation)

  def assignment = (
    ("GET" ~> "TO" ~> "THE" ~> "CHOPPER" ~> ident) <~ 
    first_op <~ 
    operations <~ 
    "ENOUGH" <~ "TALK" ^^ { varName => variables(varName) = stack.pop() }
  )

  def block = """(?s)\[.*?\]""".r ^^ {s => s.substring(1, s.length -1)}

  def iftheelse = (
    "BECAUSE" ~> "I'M" ~> "GOING" ~> "TO" ~> "SAY" ~> "PLEASE" ~> variable ~ 
    block ~ ("BULLSHIT" ~> block <~ "YOU" <~ "HAVE" <~ "NO" <~ "RESPECT" <~ "FOR" <~ "LOGIC")
  ) ^^ { case (v ~ block1 ~ block2) =>
    if (v != 0) parseAll(body, block1) else parseAll(body, block2) 
  }

  def loop = ("STICK" ~> "AROUND" ~> ident) ~ block <~ "CHILL" ^^ {case varName ~ block =>
    while (variables(varName) != 0) {
      parseAll(body, block)
    }
  }

  def print = "TALK" ~> "TO" ~> "THE" ~> "HAND" ~> (svalue|value) ^^ { println(_) }

  def statement = print | declare_var | assignment | iftheelse | loop

  def body: Parser[Any] = rep(statement)
  def main = "IT'S" ~> "SHOW" ~> "TIME" ~> body <~ "YOU" <~ "HAVE" <~ "BEEN" <~ "TERMINATED" ^^ { _ => (stack, variables)}
}

object Main {
  def main(args: Array[String]): Unit = {
    try {
      val program = Source.fromFile(args(0)).mkString
      val evaluator = new ArnoldCEvaluator()
      evaluator.parseAll(evaluator.main, program) match {
        case evaluator.Success((stk, tbl),_) => {
          println(stk)
          println("Symbol Table :-")
          tbl.foreach { s => println(f"\t$s")}
        } 
        case evaluator.Error(_,_) => println("ERROR")
        case evaluator.Failure(_,_) => println("FAILURE")
      }
    } catch {
      case ex: Exception => println(f"Can not evaluate $args(0) due to:\n$ex.toString")
    }
  }
}
