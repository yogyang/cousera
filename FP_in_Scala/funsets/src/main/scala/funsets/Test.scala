package funsets

trait Test {
  lazy val ss = {
    println("Initialize ss")
    "1"
  }
}

object AT extends Test {
  def run(): Unit ={
    this.ss
  }
}

object BT extends Test {
   def run(): Unit = {
    this.ss
  }
}

object TTTTT {
  def main(arr: Array[String]) = {
    AT.run()
    BT.run()
  }
}

class T (val t: String, s: String) {
  def test(): Unit ={
    println(s)
    println("dsfsfd")
  }
}
