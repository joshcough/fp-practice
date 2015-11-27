package answers.let

import LetLang._
import org.scalacheck.Prop._
import org.scalacheck.Properties

object LetTests extends Properties("Let") {

  test(7.n shouldBe 7)
  test(5.n + 6.n shouldBe 11)
  test(5.n * 6.n shouldBe 30)
  test((5.n * 6.n) * (5.n + 5.n) shouldBe 300)
  test((5.n + 6.n) + (5.n * 5.n) shouldBe 36)
  // let x = 9 in x
  test(Let(("x",  9.n), v"x") shouldBe 9)
  // let x = 9 in x * x
  test(Let(("x",  9.n), v"x" * v"x") shouldBe 81)
  // let x = 9 in let x = 8 in x * x
  test(Let(("x",  9.n), Let(("x",  8.n), v"x" * v"x")) shouldBe 64)
  // let x = 9 in let y = 8 in let x = 7 in y * x
  test(Let(("x",  9.n), Let(("y",  8.n), Let(("x",  7.n), v"y" * v"x"))) shouldBe 56)

  def test(t: (Exp,Int)): Unit = {
    property(t._1.toString) = secure { interp(t._1) == t._2 }
    ()
  }

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  implicit class RichExp(e:Exp) {
    def +(e2: Exp) = Add(e, e2)
    def *(e2: Exp) = Mult(e, e2)
    def shouldBe(i:Int) = (e,i)
  }

  implicit class RichInt(i:Int) {
    def n = Num(i)
  }
}