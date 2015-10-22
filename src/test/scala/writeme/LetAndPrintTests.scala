package writeme

import LetAndPrintLang._
import org.scalacheck.Prop._
import org.scalacheck.Properties

object LetAndPrintTests extends Properties("LetAndPrint") {

  def test(exp: Exp, expected: (List[Int],Int)): Unit = {
    property(exp.toString) = secure { interp(exp) == expected }
    ()
  }

  test(n"7", (Nil,7))
  test(Add (n"5", n"6"), (Nil,11))
  test(Mult(n"5", n"6"), (Nil,30))
}