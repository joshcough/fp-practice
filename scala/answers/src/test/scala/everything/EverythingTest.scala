package everything

import everything.Everything._
import org.scalacheck.Prop._
import org.scalacheck.{Prop, Properties}
import scala.language.implicitConversions
import scalaz.\/
import scalaz.syntax.either._

object EverythingNaiveInterpreterTest
  extends EverythingTest(EverythingNaiveInterpreter)

object EverythingZInterpreterTest
  extends EverythingTest(EverythingZInterpreter)

/**
  * Created by jcough on 11/29/15.
  */
abstract class EverythingTest(i: Everything.Interpreter)
  extends Properties(i.getClass.getName) with Helpers {

  implicit val interpreter = i

  // helper variables we'll use a bunch here
  val (x,y,vx,vy) = ("x","y",v"x",v"y")
  val id      = x \-> x.v
  val const   = x \-> (y \-> x.v)
  val adder   = x \-> (y \-> (x.v + y.v))
  val multer  = x \-> (y \-> (x.v * y.v))
  val sqrer   = x \-> (x.v * x.v)
  val doubler = x \-> (x.v + x.v)

  // Memory tests!
  test((mem(0) := 10.n) mustBe (List(), Map(0 -> 10.v), 0))
  test(block(mem(0) := 10.n) mustBe (List(), Map(0 -> 10.v), 0))

  test(block(
    mem(0) := 5.n,
    mem(1) := 6.n,
    mem(0) + mem(1)
  ) mustBe (List(), Map(0 -> 5.v, 1 -> 6.v), 11))

  // forall a,v: { mem(a) := v; mem(a) } = v
  test("set then get")(forAll {
    (addr: Int, value: Int) => block(
      mem(addr) := value.n, mem(addr)
    ) mustBe (List(), Map(addr -> value.v), value)
  })

  // pointers!
  // forall a1,a2,v: {mem(a1) := a2; mem(a2) := v; mem(mem(a1))} = v
  test("pointers")(
    forAll { (a1: Int, a2: Int, value: Int) => a1 != a2 ==> {
      block(
        mem(a1) := a2.n, mem(a2) := value.n, mem(mem(a1))
      ) mustBe (List(), Map(a1 -> a2.v, a2 -> value.v), value)
    }}
  )

  test("consecutive sets")(
    forAll { (a: Int, v1: Int, v2: Int) => v1 != v2 ==> {
      block(
        mem(a) := v1.n, mem(a) := v2.n, mem(a)
      ) mustBe (List(), Map(a -> v2.v), v2)
    }}
  )

  // function tests
  test(id(5)         mustBe 5)
  test(const(5)(10)  mustBe 5)
  test(const(5)(123) mustBe 5)
  test(adder(5)(5)   mustBe 10)
  test(multer(5)(5)  mustBe 25)
  test(doubler(5)    mustBe 10)
  test(sqrer(5)      mustBe 25)

  test((x \-> (7.n + x.v))(5) mustBe 12)
  test((x \-> (x.v + doubler(x.v)))(5) mustBe 15)
  test((x \-> (y \-> id))(6)(7)(8) mustBe 8)

  // TODO: tests for functions and printing.

  // tests from FirstLang
  test(7.n mustBe 7)
  test(5.n + 6.n mustBe 11)
  test(5.n * 6.n mustBe 30)
  test((5.n * 6.n) * (5.n + 5.n) mustBe 300)
  test((5.n + 6.n) + (5.n * 5.n) mustBe 36)

  // same tests, but with print
  test(Print(7.n) mustBe (List(7), Map(), 7))
  test(5.n + Print(6.n) mustBe (List(6), Map(), 11))
  test(Print(5.n)  * 6.n mustBe (List(5), Map(), 30))
  test((Print(5.n) * Print(6.n)) * (Print(5.n) + Print(5.n)) mustBe (List(5,6,5,5), Map(), 300))
  test((Print(5.n) + Print(6.n)) + (Print(5.n) * Print(5.n)) mustBe (List(5,6,5,5), Map(), 36))

  // tests from let
  test(x -> 9.n in x.v mustBe 9)
  test(x -> 9.n in x.v * x.v mustBe 81)
  test(x -> 9.n in (x -> 8.n in (x.v * x.v)) mustBe 64)
  test(x -> 9.n in (y -> 8.n in (x -> 7.n in y.v * x.v)) mustBe 56)

  // lets and prints together
  test(("x" -> 9.n in print(v"x")) mustBe (List(9), Map(), 9))
  test(("x" -> print(9.n) in v"x") mustBe (List(9), Map(), 9))
  test(("x" -> print(9.n) in print(v"x")) mustBe (List(9,9), Map(), 9))
  test(("x" -> print(9.n) in
    ("y" -> print(8.n) in
      ("x" -> print(7.n) in
        print(v"y" * v"x")))) mustBe (List(9,8,7,56), Map(), 56))

}

trait Helpers { self: Properties =>

  def test(name:String)(f: => Prop): Unit = {
    property(name) = secure { f }
    ()
  }

  implicit class RichInt(i:Int) {
    def n: Exp = Num(i)
    def v: RuntimeValue = NumV(i)
  }

  def test(t: (Exp,Output,Mem,RuntimeValue))
          (implicit i: Everything.Interpreter): Unit = {
    test(t._1.toString)(testBody(t))
  }

  def testBody(t: (Exp,Output,Mem,RuntimeValue))
              (implicit i: Everything.Interpreter): Prop =
    secure {
      val v: (String \/ RuntimeValue, PState) = i.interpret(t._1)
      v == (t._4.right -> PState(Map(), t._2, t._3))
    }

  implicit def tProp(t: (Exp,Output,Mem,RuntimeValue))
                    (implicit i: Everything.Interpreter): Prop = testBody(t)

  implicit class RichString(s:String) {
    def fun(exp: Exp): Exp = Function(s, exp)
    def v: Exp = Var(s)
    def \->(e:Exp) = Function(s, e)
    def +(e2: Exp) = Add(Var(s), e2)
    def *(e2: Exp) = Mult(Var(s), e2)
  }

  implicit class RichExp(e:Exp) {
    def +(e2: Exp) = Add(e, e2)
    def *(e2: Exp) = Mult(e, e2)
    def mustBe(i:Int): (Exp, Output, Mem, RuntimeValue) =
      (e, List(), Map(), NumV(i))
    def mustBe(l:List[Int], m: Mem, i:Int): (Exp, Output, Mem, RuntimeValue) =
      (e,l.map(_.toString),m, NumV(i))
  }

  def print(e:Exp) = Print(e)

  implicit class RichWhat(t: (String, Exp)) {
    def in(e:Exp) = Let(t,e)
  }

  implicit class RichGetMem(e:GetMem) {
    def := (exp:Exp) = SetMem(e.address,exp)
  }

  def block(e:Exp*) = Statements(e.toList)

  def mem(i: Int): GetMem = GetMem(i.n)
  def mem(e: Exp): GetMem = GetMem(e)
}
