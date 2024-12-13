class Editor {
  private var cursorPos: Int = -1
  private var str: String = ""

  def x(): Unit = {
    if (str.isEmpty || cursorPos == -1) {
      display()
      return
    }

    if (str.length == cursorPos) {
      cursorPos -= 1      
      str = str.slice(0, cursorPos)
    } else {
      val (l, r) = str.splitAt(cursorPos-1) 
      val r1 = r.slice(1, r.length)
      str = l + r1
    }
    display()
  }

  def dw(): Unit = {
    if (str.isEmpty || cursorPos == -1) {
      display()
      return
    }

    val (l, r) = str.splitAt(cursorPos-1)
    var idx = r.indexOf(' ', 0)
    if (idx == -1) {
      str = l
      cursorPos = str.length
    } else {
      str = l + r.slice(idx + 1, r.length)
    }
    display()
  }

  def i(c: Char): Unit = {
    if (cursorPos == -1) {
      str += c
      cursorPos = 1
    } else {
      val (left, right) = str.splitAt(cursorPos)
      cursorPos += 1
      str = left + c + right
    }
    display() 
  }

  def iw(word: String): Unit = {
    if (cursorPos == -1) {
      str += s"$word "
      cursorPos = word.length + 1
    } else {
      val (left, right) = str.splitAt(cursorPos)
      val w = s"$word "
      cursorPos += w.length 
      str = left + w + right
    }
    display() 
  }

  def l(n: Int = 1): Unit = {
    if (!str.isEmpty) 
      cursorPos = if (cursorPos + n > str.length) str.length else cursorPos + n 
    display()
  }

  def h(n: Int = 1): Unit = {
    if (!str.isEmpty)
      cursorPos = if (cursorPos - n <= 0) 1 else cursorPos - n
    display()
  }

  def display(): Unit = {
    println(str)
  }
  
  protected def cursor() = cursorPos
  protected def line() = str 
}

trait Debug extends Editor {
  private var lastLine: String = super.line()
  private var lastCmd: String = ""

  override def x(): Unit = {
    lastLine = super.line()
    lastCmd = "x"
    super.x()
  }

  override def dw(): Unit = {
    lastLine = super.line()
    lastCmd = "dw"
    super.dw()
  }

  override def i(c: Char): Unit = {
    lastLine = super.line()
    lastCmd = "i"
    super.i(c)
  }

  override def iw(word: String): Unit = {
    lastLine = super.line()
    lastCmd = "iw"
    super.iw(word)
  }

  override def l(n: Int = 1): Unit = {
    lastLine = super.line()
    lastCmd = "l"
    super.l(n)
  }

  override def h(n: Int = 1): Unit = {
    lastLine = super.line()
    lastCmd = "h"
    super.h(n)
  }

  override def display(): Unit = {
    println(s"[BEFORE] -: \"${lastLine}\"")
    var db = s"[${lastCmd}] "
    print(db)
    super.display()
    (1 until (db.length + super.line().length + 1))
      .foreach(i => if (i-db.length == super.cursor()) print("^") else print(" "))
    println()
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    test(new Editor) 
    test(new Editor with Debug)
  }

  def test(ex: Editor) = {
    ex i ' '
    ex.x()
    ex i 'h'
    ex iw "ello world"
    ex iw "!! Nice"
    ex.h(2)
    ex.h(12)
    ex.dw()
    ex.l()
    ex.l()
    ex.x()
    ex.l(20)
    ex.x()
    ex.h(10)
    ex.x()
    ex.l()
    ex.dw()
    ex.dw()
    ex.dw()
    ex.h(10)
    ex.dw()
    ex iw "Hello world"
    ex.h(20)
    ex.dw()
  }
}
