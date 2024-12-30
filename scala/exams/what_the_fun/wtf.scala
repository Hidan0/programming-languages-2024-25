import scala.util.parsing.combinator._
import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

class WtFEvaluator(
    var args: Array[Int] = Array.empty, 
    var tbl: HashMap[Char, (Int, String)] = HashMap.empty, 
    var mem: Stack[Int] = Stack.empty) extends JavaTokenParsers {

  // BASE
  def value = "0" ^^ {_ => mem.push(0)}

  def op = ("+"|"-") ^^ {
    case "+" => mem.push(mem.pop()+1)
    case "-" => mem.push(mem.pop()-1)
  }

  def print = "!" ^^ {_ => println(mem.pop())}

  def block = """\[.*?\]""".r ^^ {b => b.substring(1, b.length-1)}
  def ifelse: Parser[Any] = "?" ~> block ~ (":" ~> block) ^^ {case bl1 ~ bl2 => if (mem.pop() == 0) parseAll(body, bl1) else parseAll(body, bl2)}

  // FUNCTIONS
  def f_value = """\$[0-9]+""".r ^^ {v => 
    val idx = v.substring(1, v.length).toInt - 1
    mem.push(args(idx))
  }

  def f_name = """[A-Z]""".r ^^ {_.charAt(0)}
  def f_def = "def" ~> f_name ~ wholeNumber ~ ("=" ~> """.*\n""".r) ^^ {case name ~ args ~ b => 
    tbl(name) = (args.toInt, b.dropRight(1))
  } 

  def f_call = f_name ^^ {name =>
    val (argc, b) = tbl(name)
    val local_args = {argc to 1 by -1}.map {_ => mem.pop()}.toArray.reverse
    val innerP = new WtFEvaluator(local_args, tbl, mem)
    innerP.parseAll(innerP.body, b)
  }

  // PROGRAM 
  def def_sect = rep(f_def)

  def expr = value|f_value|print|ifelse|f_call|op
  def body: Parser[Any] = expr ~ rep(expr)

  def program = def_sect ~ body
}

object Main {
  def main(args: Array[String]): Unit = {
    try {
      val src = Source.fromFile(args(0)).mkString
      val p = new WtFEvaluator()
      p.parseAll(p.program, src) match {
        case p.Success(_, _) => println("OK")
        case p.Failure(e, _) => println(f"*** FAILURE:\n$e.toString")
        case p.Error(e, _) => println(f"*** ERROR:\n$e.toString")
      }
    } catch {
      case ex: Exception => println(f"Can not parse $args(0) due to:\n$ex.toString")
    }
  }
}
