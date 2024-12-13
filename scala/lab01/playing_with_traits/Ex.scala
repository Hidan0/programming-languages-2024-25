class Editor {
  private var cursorPos: Int = -1
  private var line: String = ""

  def x(): Unit = {
    if (line.isEmpty || cursorPos == -1)
      return

    if (line.length == cursorPos) {
      cursorPos -= 1      
      line = line.slice(0, cursorPos)
      return
    }

    val (l, r) = line.splitAt(cursorPos-1) 
    val r1 = r.slice(1, r.length)
    line = l + r1
  }

  def dw(): Unit = {
    if (line.isEmpty || cursorPos == -1)
      return

    val (l, r) = line.splitAt(cursorPos-1)
    var idx = r.indexOf(' ', 0)
    if (idx == -1) {
      line = l
      cursorPos = line.length
      return
    }
    line = l + r.slice(idx + 1, r.length)
  }

  def i(c: Char): Unit = {
    if (cursorPos == -1) {
      line += c
      cursorPos = 1
      return
    }
    
    val (left, right) = line.splitAt(cursorPos)
    cursorPos += 1
    line = left + c + right
  }

  def iw(word: String): Unit = {
    if (cursorPos == -1) {
      line += s"$word "
      cursorPos = word.length + 1
      return
    }
    
    val (left, right) = line.splitAt(cursorPos)
    val w = s"$word "
    cursorPos += w.length 
    line = left + w + right
  }

  def l(n: Int = 1): Unit = {
    if (line.isEmpty) return 
    cursorPos = if (cursorPos + n > line.length) line.length else cursorPos + n 
  }

  def h(n: Int = 1): Unit = {
    if (line.isEmpty) return 
    cursorPos = if (cursorPos - n <= 0) 1 else cursorPos - n
  }

  def display(): Unit = {
    if (line.isEmpty) {
      println("EMPTY")
      return
    }
    println(line)
    (1 until line.length + 1)
      .foreach(i => if (i == cursorPos) print("^") else print(" "))
    println()
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val ex = new Editor
    ex.display()
    ex i ' '
    ex.display()
    ex.x()
    ex.display()
    ex i 'h'
    ex.display()
    ex iw "ello world"
    ex iw "!! Nice"
    ex.display()
    ex.h(2)
    ex.display()
    ex.h(12)
    ex.display()
    ex.dw()
    ex.display()
    ex.l()
    ex.l()
    ex.display()
    ex.x()
    ex.display()
    ex.l(20)
    ex.display()
    ex.x()
    ex.display()
    ex.h(10)
    ex.x()
    ex.display()
    ex.l()
    ex.dw()
    ex.display()
    ex.dw()
    ex.display()
    ex.dw()
    ex.display()
    ex.h(10)
    ex.dw()
    ex.display()
    ex iw "Hello world"
    ex.display()
    ex.h(20)
    ex.display()
    ex.dw() 
    ex.display()
  }
}
