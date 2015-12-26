package letAndPrint

import LetAndPrint._
import org.scalacheck.Prop._
import org.scalacheck.Properties

object LetAndPrintNaiveTests extends
  LetAndPrintTests(LetAndPrintNaive)

object LetAndPrintWriterTests extends
  LetAndPrintTests(LetAndPrint_Writer)

object LetAndPrintReaderTWriterTests extends
  LetAndPrintTests(LetAndPrint_ReaderT_Writer)

object LetAndPrint_WriterT_ReaderTests extends
  LetAndPrintTests(LetAndPrint_WriterT_Reader)

object LetAndPrint_EitherT_ReaderT_WriterTests extends
  LetAndPrintTests(LetAndPrint_EitherT_ReaderT_Writer)

object LetAndPrint_EitherT_ReaderT_WriterT_StateTests extends
  LetAndPrintTests(LetAndPrint_EitherT_ReaderT_WriterT_State)

abstract class LetAndPrintTests(interp: Interpreter)
  extends Properties(interp.getClass.getName) {

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
  test("x" -> 9.n in v"x" mustBe (Nil, 9))
  test("x" -> 9.n in v"x" * v"x" mustBe (Nil, 81))
  test("x" -> 9.n in ("x" ->  8.n in (v"x" * v"x")) mustBe (Nil, 64))
  test("x" -> 9.n in ("y" ->  8.n in ("x" -> 7.n in v"y" * v"x")) mustBe (Nil, 56))

  // lets and prints together
  test(("x" -> 9.n in print(v"x")) mustBe (List(9), 9))
  test(("x" -> print(9.n) in v"x") mustBe (List(9), 9))
  test(("x" -> print(9.n) in print(v"x")) mustBe (List(9,9), 9))
  test(("x" -> print(9.n) in
    ("y" -> print(8.n) in
      ("x" -> print(7.n) in
        print(v"y" * v"x")))) mustBe (List(9,8,7,56), 56))

  // test statements
  test("x" -> print(9.n) in block(print(v"x")) mustBe (List(9,9), 9))
  /*
    let x = print 0 in
      print 2
      print 1
      print x
   */
  test("x" -> print(0.n) in block(
    print(2.n),
    print(1.n),
    print(v"x")) mustBe (List(0,2,1,0), 0))

  def block(e:Exp*) = Statements(e.toList)
  def test(t: (Exp,List[Int],Int)): Unit = {
    property(t._1.toString) = secure {
      val exp: Exp = t._1
      val list: List[String] = t._2.map(_.toString)
      val res: Int = t._3
      // TODO: abstract over interpreter
      interp.interpret(exp) ?= (list -> res)
    }
    ()
  }
}
