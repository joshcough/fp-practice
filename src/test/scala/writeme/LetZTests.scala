package writeme

import LetZLang._

object LetZTests {
  def main(args:Array[String]): Unit = {
    run(n"7", 7)
    run(Add (n"5", n"6"), 11)
    run(Mult(n"5", n"6"), 30)
    run(Let(("x",  n"5"), v"x"), 5)
    run(Let(("x",  n"9"), Mult(v"x", v"x")), 81)
    println("success!!")
  }
}
