package answers.print

import answers.print.LetAndPrintStatementBlocks._
import org.scalacheck.Prop._
import org.scalacheck.Properties

object LetAndPrintStatementBlocksTests extends Properties("LetAndPrintStatementBlocksTests") {

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
  test("x" -> 9.n in v"x" shouldBe (Nil, 9))
  test("x" -> 9.n in v"x" * v"x" shouldBe (Nil, 81))
  test("x" -> 9.n in ("x" ->  8.n in (v"x" * v"x")) shouldBe (Nil, 64))
  test("x" -> 9.n in ("y" ->  8.n in ("x" -> 7.n in v"y" * v"x")) shouldBe (Nil, 56))

  // lets and prints together
  test("x" -> 9.n in print(v"x") shouldBe (List(9), 9))
  test("x" -> print(9.n) in v"x" shouldBe (List(9), 9))
  test("x" -> print(9.n) in print(v"x") shouldBe (List(9,9), 9))
  test(("x" -> print(9.n) in
         ("y" -> print(8.n) in
           ("x" -> print(7.n) in
             print(v"y" * v"x")))) shouldBe (List(9,8,7,56), 56))

  // test statements
  test("x" -> print(9.n) in block(print(v"x")) shouldBe (List(9,9), 9))
  /*
    let x = print 0 in
      print 2
      print 1
      print x
   */
  test("x" -> print(0.n) in block(
        print(2.n),
        print(1.n),
        print(v"x")) shouldBe (List(0,2,1,0), 0))

  def block(e:Exp*) = Statements(e.toList)

  def test(t: (Exp,List[Int],Int)): Unit = {
    property(t._1.toString) = secure {
      val expr: Exp = t._1
      val output: List[String] = t._2.map(_.toString)
      val expectedRes: Int = t._3
      val actual = interp(expr)
      actual == (output -> expectedRes)
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

  implicit class RichWhat(t: (String, Exp)) {
    def in(e:Exp) = Let(t,e)
  }

  def print(e:Exp) = Print(e)

  implicit class RichInt(i:Int) {
    def n = Num(i)
  }
}