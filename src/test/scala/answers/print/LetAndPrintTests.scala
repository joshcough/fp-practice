package answers.print

import answers.print.LetAndPrint._
import org.scalacheck.Prop._
import org.scalacheck.Properties

object LetAndPrintTests extends Properties("LetAndPrint") {

  // same tests from FirstLang
  test(7.n shouldBe (Nil, 7))
  test(5.n + 6.n shouldBe (Nil, 11))
  test(5.n * 6.n shouldBe (Nil, 30))
  test((5.n * 6.n) * (5.n + 5.n) shouldBe (Nil, 300))
  test((5.n + 6.n) + (5.n * 5.n) shouldBe (Nil, 36))

  // same tests, but with print
  test(Print(7.n) shouldBe (List(7), 7))
  test(5.n + Print(6.n) shouldBe (List(6), 11))
  test(Print(5.n)  * 6.n shouldBe (List(5), 30))
  test((Print(5.n) * Print(6.n)) * (Print(5.n) + Print(5.n)) shouldBe (List(5,6,5,5), 300))
  test((Print(5.n) + Print(6.n)) + (Print(5.n) * Print(5.n)) shouldBe (List(5,6,5,5), 36))

  // tests from let
  // let x = 9 in x
  test(Let(("x",  9.n), v"x") shouldBe (Nil, 9))
  // let x = 9 in x * x
  test(Let(("x",  9.n), v"x" * v"x") shouldBe (Nil, 81))
  // let x = 9 in let x = 8 in x * x
  test(Let(("x",  9.n), Let(("x",  8.n), v"x" * v"x")) shouldBe (Nil, 64))
  // let x = 9 in let y = 8 in let x = 7 in y * x
  test(Let(("x",  9.n), Let(("y",  8.n), Let(("x",  7.n), v"y" * v"x"))) shouldBe (Nil, 56))

  // lets and prints together
  // let x = 9 in print x
  test(Let(("x",  9.n), Print(v"x")) shouldBe (List(9), 9))
  // let x = print 9 in x
  test(Let(("x",  Print(9.n)), v"x") shouldBe (List(9), 9))
  // let x = print 9 in print x
  test(Let(("x",  Print(9.n)), Print(v"x")) shouldBe (List(9,9), 9))

  // let x = print 9 in
  //   let y = print 8 in
  //      let x = print 7 in
  //         print(y * x)
  test(
    Let(("x",  Print(9.n)),
      Let(("y",  Print(8.n)),
        Let(("x",  Print(7.n)),
          Print(v"y" * v"x")))) shouldBe (List(9,8,7,56), 56))

  def test(t: (Exp,List[Int],Int)): Unit = {
    property(t._1.toString) = secure {
      val exp: Exp = t._1
      val list: List[String] = t._2.map(_.toString)
      val res: Int = t._3
      interp(exp) == (list -> res)
    }
    ()
  }

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  implicit class RichExp(e:Exp) {
    def +(e2: Exp) = Add(e, e2)
    def *(e2: Exp) = Mult(e, e2)
    def shouldBe(printed: List[Int], value: Int) = (e, printed, value)
  }

  implicit class RichInt(i:Int) {
    def n = Num(i)
  }
}