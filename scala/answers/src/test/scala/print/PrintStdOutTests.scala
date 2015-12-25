package print

import org.scalacheck.Prop._
import org.scalacheck.Properties
import PrintAST._
import PrintStdOut._

object PrintStdOutTests extends Properties("PrintStdOut") {

  // tests from FirstLang
  test(7.n mustBe 7)
  test(5.n + 6.n mustBe 11)
  test(5.n * 6.n mustBe 30)
  test((5.n * 6.n) * (5.n + 5.n) mustBe 300)
  test((5.n + 6.n) + (5.n * 5.n) mustBe 36)

  // same tests, but with print
  test(Print(7.n) mustBe 7)
  test(5.n + Print(6.n) mustBe 11)
  test(Print(5.n) * 6.n mustBe 30)
  test((Print(5.n) * Print(6.n)) * (Print(5.n) + Print(5.n)) mustBe 300)
  test((Print(5.n) + Print(6.n)) + (Print(5.n) * Print(5.n)) mustBe 36)

  def test(t: (Exp,Int)): Unit = {
    property(t._1.toString) = secure { eval(t._1) == t._2 }
    ()
  }

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  implicit class RichExp(e:Exp) {
    def +(e2: Exp) = Add(e, e2)
    def *(e2: Exp) = Mult(e, e2)
    def mustBe(value: Int) = (e, value)
  }

  implicit class RichInt(i:Int) {
    def n = Num(i)
  }
}