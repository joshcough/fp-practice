package answers.print

import org.scalacheck.Prop._
import org.scalacheck.Properties
import PrintStdOut._

object PrintStdOutTests extends Properties("PrintStdOut") {

  // same tests from FirstLang
  test(7.n shouldBe 7)
  test(5.n + 6.n shouldBe 11)
  test(5.n * 6.n shouldBe 30)
  test((5.n * 6.n) * (5.n + 5.n) shouldBe 300)
  test((5.n + 6.n) + (5.n * 5.n) shouldBe 36)

  // same tests, but with print
  test(Print(7.n) shouldBe 7)
  test(5.n + Print(6.n) shouldBe 11)
  test(Print(5.n) * 6.n shouldBe 30)
  test((Print(5.n) * Print(6.n)) * (Print(5.n) + Print(5.n)) shouldBe 300)
  test((Print(5.n) + Print(6.n)) + (Print(5.n) * Print(5.n)) shouldBe 36)

  def test(t: (Exp,Int)): Unit = {
    property(t._1.toString) = secure { interp(t._1) == t._2 }
    ()
  }

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  implicit class RichExp(e:Exp) {
    def +(e2: Exp) = Add(e, e2)
    def *(e2: Exp) = Mult(e, e2)
    def shouldBe(value: Int) = (e, value)
  }

  implicit class RichInt(i:Int) {
    def n = Num(i)
  }
}