package functions

import functions.FunctionsAndPrint._
import org.scalacheck.Prop._
import org.scalacheck.Properties

/**
  * Created by jcough on 11/29/15.
  */
object FunctionsAndPrintTest extends Properties("FunctionsTest") {

  val (x,y)   = ("x", "y")
  val id      = x \-> x.v
  val const   = x \-> (y \-> x.v)
  val adder   = x \-> (y \-> (x.v + y.v))
  val multer  = x \-> (y \-> (x.v * y.v))
  val sqrer   = x \-> (x.v * x.v)
  val doubler = x \-> (x.v + x.v)

  // function tests
  test(id(5)         mustBe (Nil, 5))
  test(const(5)(10)  mustBe (Nil, 5))
  test(const(5)(123) mustBe (Nil, 5))
  test(adder(5)(5)   mustBe (Nil, 10))
  test(multer(5)(5)  mustBe (Nil, 25))
  test(doubler(5)    mustBe (Nil, 10))
  test(sqrer(5)      mustBe (Nil, 25))

  test((x \-> (7.n + x.v))(5) mustBe (Nil, 12))
  test((x \-> (x.v + doubler(x.v)))(5) mustBe (Nil, 15))
  test((x \-> (y \-> id))(6)(7)(8) mustBe (Nil, 8))

  // TODO: tests for functions and printing.

  // tests from FirstLang
  test(7.n mustBe (Nil, 7))
  test(5.n + 6.n mustBe (Nil, 11))
  test(5.n * 6.n mustBe (Nil, 30))
  test((5.n * 6.n) * (5.n + 5.n) mustBe (Nil, 300))
  test((5.n + 6.n) + (5.n * 5.n) mustBe (Nil, 36))

  // same tests, but with print
  test(Print(7.n) mustBe (List(7), 7))
  test(5.n + Print(6.n) mustBe (List(6), 11))
  test(Print(5.n)  * 6.n mustBe (List(5), 30))
  test((Print(5.n) * Print(6.n)) * (Print(5.n) + Print(5.n)) mustBe (List(5,6,5,5), 300))
  test((Print(5.n) + Print(6.n)) + (Print(5.n) * Print(5.n)) mustBe (List(5,6,5,5), 36))

  // tests from let
  test(x -> 9.n in x.v mustBe (Nil, 9))
  test(x -> 9.n in x.v * x.v mustBe (Nil, 81))
  test(x -> 9.n in (x -> 8.n in (x.v * x.v)) mustBe (Nil, 64))
  test(x -> 9.n in (y -> 8.n in (x -> 7.n in y.v * x.v)) mustBe (Nil, 56))

  // lets and prints together
  test(("x" -> 9.n in print(v"x")) mustBe (List(9), 9))
  test(("x" -> print(9.n) in v"x") mustBe (List(9), 9))
  test(("x" -> print(9.n) in print(v"x")) mustBe (List(9,9), 9))
  test(("x" -> print(9.n) in
    ("y" -> print(8.n) in
      ("x" -> print(7.n) in
        print(v"y" * v"x")))) mustBe (List(9,8,7,56), 56))

  def test(t: (Exp,Output,RuntimeValue)): Unit = {
    property(t._1.toString) = secure { interp(t._1) == (t._2 -> t._3) }
    ()
  }

  implicit class RichInt(i:Int) {
    def n: Exp = Num(i)
  }

  implicit class RichString(s:String) {
    def fun(exp: Exp): Exp = Function(s, exp)
    def v: Exp = Var(s)
    def \->(e:Exp) = Function(s, e)
    def +(e2: Exp) = Add(Var(s), e2)
    def *(e2: Exp) = Mult(Var(s), e2)
  }

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  implicit class RichExp(e:Exp) {
    def +(e2: Exp) = Add(e, e2)
    def *(e2: Exp) = Mult(e, e2)
    def mustBe(l:List[Int], i:Int): (Exp, Output, RuntimeValue) =
      (e,l.map(_.toString),NumV(i))
  }

  def print(e:Exp) = Print(e)

  implicit class RichWhat(t: (String, Exp)) {
    def in(e:Exp) = Let(t,e)
  }
}