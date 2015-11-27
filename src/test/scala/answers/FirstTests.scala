package answers

import org.scalacheck.Prop._
import org.scalacheck.Properties
import FirstLang._

object FirstTests extends Properties("First") {

  test(7.n shouldBe 7)
  test(5.n + 6.n shouldBe 11)
  test(5.n * 6.n shouldBe 30)
  test((5.n * 6.n) * (5.n + 5.n) shouldBe 300)
  test((5.n + 6.n) + (5.n * 5.n) shouldBe 36)

  def test(t: (Exp,Int)): Unit = {
    property(t._1.toString) = secure { interp(t._1) == t._2 }
    ()
  }
}