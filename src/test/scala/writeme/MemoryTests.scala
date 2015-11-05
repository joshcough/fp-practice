package writeme

import MemoryLang._

object MemoryTests {
  def main(args:Array[String]): Unit = {
    run(n"7", (7, Map()))
    run(Add (n"5", n"6"), (11, Map()))
    run(Mult(n"5", n"6"), (30, Map()))
    run(Let(("x",  n"5"), v"x"), (5, Map()))
    run(Let(("x",  n"9"), Mult(v"x", v"x")), (81, Map()))
    println(interp(SetMem(0, n"10")))
    println("success!!")
  }
}

