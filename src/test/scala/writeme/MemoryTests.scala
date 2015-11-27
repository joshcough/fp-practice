package writeme

import MemoryLang._

object MemoryTests {
  def main(args:Array[String]): Unit = {
    println(interp(SetMem(0, n"10")))
  }
}

