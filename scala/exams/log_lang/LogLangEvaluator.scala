import scala.util.parsing.combinator._
import scala.io.Source
import java.nio.file.Files
import java.nio.file.Paths
import scala.util.Try

object LogLangEvaluator extends JavaTokenParsers {

  sealed trait Cmd {
    def eval: Boolean 
  }
  case class Remove(file: String) extends Cmd {
    override def eval = {
      Try {
        Files.delete(Paths.get(file))
      }.isSuccess
    }
  }
  case class Rename(from: String, to: String) extends Cmd {
    override def eval = {
      Try {
        Files.move(Paths.get(from), Paths.get(to))
      }.isSuccess
    }  
  }
  case class Merge(f1: String, f2: String, f3: String) extends Cmd {
    override def eval = {
      Try {
        val s1 = Source.fromFile(f1).mkString
        val s2 = Source.fromFile(f2).mkString
        val data = f"${s1}\n${s2}"
        Files.write(Paths.get(f3), data.getBytes)
      }.isSuccess
    }
  }
  case class Backup(from: String, to: String) extends Cmd {
    override def eval = {
      Try {
        Files.copy(Paths.get(from), Paths.get(to))
      }.isSuccess
    }
  }

  class Task(name: String, cmds: List[Cmd]) extends Cmd {
    override def eval = {
      println(f"Task $name")
      cmds.zipWithIndex.foreach { case (cmd, i) =>
        val res: Boolean = cmd.eval
        println(f"  [op${i+1}] ${res.toString.toLowerCase}")
      }
      true
    } 
  }

  def args = rep(stringLiteral) ^^ { a =>
    println("*** Got args: ")
    a.foreach { ag => println(f"    $ag") } 
    a.map(s => s.substring(1, s.length-1))
  }
  def cmd = ("remove"|"rename"|"merge"|"backup") ~ args ^^ {
    case "remove" ~ a => {
      println("*** Got cmd remove")
      if (a.length < 1) throw new IllegalArgumentException("Not enough args")
      Remove(a(0))
    }
    case "rename" ~ a => {
      println("*** Got cmd rename")
      if (a.length < 2) throw new IllegalArgumentException("Not enough args")
      Rename(a(0), a(1))
    }
    case "merge"  ~ a => {
      println("*** Got cmd merge")
      if (a.length < 3) throw new IllegalArgumentException("Not enough args")
      Merge(a(0), a(1), a(2))
    }
    case "backup" ~ a => {
      println("*** Got cmd backup")
      if (a.length < 2) throw new IllegalArgumentException("Not enough args")
      Backup(a(0), a(1))
    }
    case c ~ _ => throw new IllegalArgumentException(f"Unknown command $c") 
  }

  def cmds = rep(cmd) ^^ { c =>
    println(f"*** Got cmds with size ${c.length}: ")
    c.foreach { cd => println(f"    $cd") } 
    c
  }

  def task: Parser[Task] = ("task" ~> """[a-zA-Z]+""".r) ~ ("{" ~> cmds <~ "}") ^^ {
    case name ~ tasks => {
      println(f"*** Got task with name $name and $tasks.size commdans")
      new Task(name, tasks)
    }
  }

  def script: Parser[List[Task]] = rep(task) ^^ { tks =>
    println(f"*** Got tasks with size ${tks.length}")
    tks
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    try {
      val ll = Source.fromFile(args(0)).mkString
      val script = LogLangEvaluator.parseAll(LogLangEvaluator.script, ll) match {
        case LogLangEvaluator.Success(s, _) => s
        case er => throw new RuntimeException(f"Failed to parse LogLang script due to:\n $er.toString")
      }
      script.foreach {_.eval}
    } catch {
      case e: Exception => println(f"Can not parse ${args(0)} due to\n $e.toString")
    }
  }
}
