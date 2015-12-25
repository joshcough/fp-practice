package let

import LetLang._
import org.scalacheck.Prop._
import org.scalacheck.Properties

object LetTests extends Properties("Let") {

  test(7.n mustBe 7)
  test(5.n + 6.n mustBe 11)
  test(5.n * 6.n mustBe 30)
  test((5.n * 6.n) * (5.n + 5.n) mustBe 300)
  test((5.n + 6.n) + (5.n * 5.n) mustBe 36)
  // let x = 9 in x
  test(Let(("x",  9.n), v"x") mustBe 9)
  // let x = 9 in x * x
  test(Let(("x",  9.n), v"x" * v"x") mustBe 81)
  // let x = 9 in let x = 8 in x * x
  test(Let(("x",  9.n), Let(("x",  8.n), v"x" * v"x")) mustBe 64)
  // let x = 9 in let y = 8 in let x = 7 in y * x
  test(Let(("x",  9.n), Let(("y",  8.n), Let(("x",  7.n), v"y" * v"x"))) mustBe 56)

  def test(t: (Exp,Int)): Unit = {
    property(t._1.toString) = secure { eval(t._1) == t._2 }
    ()
  }

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  implicit class RichExp(e:Exp) {
    def +(e2: Exp) = Add(e, e2)
    def *(e2: Exp) = Mult(e, e2)
    def mustBe(i:Int) = (e,i)
  }

  implicit class RichInt(i:Int) {
    def n = Num(i)
  }
}