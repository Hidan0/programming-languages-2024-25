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
  protected def cursor(pos: Int) = cursorPos = pos
  protected def line(content: String) = str = content
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

case class EditorState(str: String, cursorPos: Int) 

trait UndoRedo extends Editor {
  private var undoStack: List[EditorState] = List.empty 
  private var redoStack: List[EditorState] = List.empty 

  def saveState(): Unit = {
    undoStack = EditorState(super.line(), super.cursor()) :: undoStack
    redoStack = List.empty 
  }

  def u(): Unit = {
    if (undoStack.nonEmpty) {
      val current = EditorState(super.line(), super.cursor())
      redoStack = current :: redoStack
      val lastState = undoStack.head
      undoStack = undoStack.tail
      super.line(lastState.str)
      super.cursor(lastState.cursorPos)
    }
  }

  def ctrlr(): Unit = {
    if (redoStack.nonEmpty) {
      val current = EditorState(super.line(), super.cursor())
      undoStack = current :: undoStack
      val lastRedoState = redoStack.head
      redoStack = redoStack.tail
      super.line(lastRedoState.str)
      super.cursor(lastRedoState.cursorPos)
    }
  }

  override def x(): Unit = {
    saveState()
    super.x()
  }

  override def dw(): Unit = {
    saveState()
    super.dw()
  }

  override def i(c: Char): Unit = {
    saveState()
    super.i(c)
  }

  override def iw(word: String): Unit = {
    saveState()
    super.iw(word)
  }

  override def l(n: Int = 1): Unit = {
    saveState()
    super.l(n)
  }

  override def h(n: Int = 1): Unit = {
    saveState()
    super.h(n)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    println("*** EDITOR ***")
    test(new Editor) 
    println("*** EDITOR WITH DEBUG ***")
    test(new Editor with Debug)
    println("*** EDITOR WITH UNDOREDO ***")
    test2(new Editor with Debug with UndoRedo)
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

  def test2(ex: Editor with UndoRedo) = {
    ex.display()
    ex.u()
    ex.u()
    ex i 'h'
    ex i 'e'
    ex i 'l'
    ex i 'l'
    ex i 'o'
    ex.u()
    ex.u()
    ex.display()
    ex.ctrlr()
    ex.ctrlr()
    ex.display()
    ex iw " world"
    ex.u()
    ex.ctrlr()
  }
}
