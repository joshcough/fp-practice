package memory

import MemoryLang._

object MemoryTests {
  def main(args:Array[String]): Unit = {
    println(eval(SetMem(0, n"10")))
  }
}

