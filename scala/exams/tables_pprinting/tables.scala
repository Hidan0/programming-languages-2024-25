import scala.util.parsing.combinator._
import scala.io.Source

class CSVParserCombinator extends JavaTokenParsers {
  override val skipWhitespace = false
  private var columnsNum: Option[Int] = Option.empty 

  def quotedField = stringLiteral ^^ {_.trim}
  def unquotedField = """[^,\n]*""".r ^^ {_.trim}

  def field: Parser[String] = quotedField | unquotedField

  def row: Parser[List[String]] = repsep(field, ",") ^^ { case fields=>
    columnsNum match {
      case None => {
        columnsNum = Some(fields.length)
        fields 
      }
      case Some(cNum) => {
        if (fields.length > 1 && fields.length != cNum) {
          throw new IllegalArgumentException(f"Rows has an incorrect number of fields")
        }

        fields
      }
    }
  }

  def csv: Parser[List[List[String]]] = repsep(row, "\n") ^^ { case rows =>
    rows.dropRight(1)
  }
}

object PrettyPrintCSV {
  private def findMaxLen(l: List[String]): Int = {
    l.map{_.length}.max
  }

  private def printRow(row: List[String], width: List[Int]) = {
    row.zip(width).foreach { case (e,w) =>
      printf(f"| %%-${w}s ", e)
    }
    println("|")
  }

  def pprint(table: List[List[String]]): Unit = {
    val head = table(0)
    val cols = table.transpose

    val colsWidth = cols.map{col => col.maxBy(_.length).length}

    val tableWidth = colsWidth.map {_+3}.sum + 1
    println("-" * tableWidth)
    printRow(head, colsWidth)
    println("-" * tableWidth)
    table.tail.foreach{printRow(_, colsWidth)}
    println("-" * tableWidth)
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    args.foreach { f =>
      try {
        val csv = Source.fromFile(f).mkString
        val p = new CSVParserCombinator()

        p.parseAll(p.csv, csv) match {
          case p.Success(parsed, _) => {
            PrettyPrintCSV.pprint(parsed)
            println()
          }
          case e => println(f"Something went wrong: ${e.toString}")
        }
      } catch {
        case ex: Exception => println(f"Failed to parse $f due to ${ex.toString}")
      }
    }
  }
}
