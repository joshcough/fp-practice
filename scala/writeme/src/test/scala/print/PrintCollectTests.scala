package print

import PrintCollect._
import org.scalacheck.Prop._
import org.scalacheck.Properties

object PrintCollectTests extends Properties("PrintCollect") {

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

  def test(t: (Exp,List[Int],Int)): Unit = {
    property(t._1.toString) = secure {
      val exp: Exp = t._1
      val list: List[String] = t._2.map(_.toString)
      val res: Int = t._3
      eval(exp) == (list -> res)
    }
    ()
  }

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  implicit class RichExp(e:Exp) {
    def +(e2: Exp) = Add(e, e2)
    def *(e2: Exp) = Mult(e, e2)
    def mustBe(printed: List[Int], value: Int) = (e, printed, value)
  }

  implicit class RichInt(i:Int) {
    def n = Num(i)
  }
}