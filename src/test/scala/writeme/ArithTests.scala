package writeme

import ArithLang._
import org.scalacheck.Prop._
import org.scalacheck.Properties

object ArithTests extends Properties("Arith") {

  def test(exp: Exp, expected: Int): Unit = {
    property(exp.toString) = secure { interp(exp) == expected }
    ()
  }

  test(n"7", 7)
  test(Add (n"5", n"6"), 11)
  test(Mult(n"5", n"6"), 30)
}