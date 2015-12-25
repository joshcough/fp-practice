package memory

import MemoryLang._
import org.scalacheck.Prop._
import org.scalacheck.{Prop, Properties}
import scala.language.implicitConversions

/**
  * All the tests from FirstLang and LetLang should pass,
  * since MemoryLang has Let and arithmetic. Those tests
  * have all been put on the bottom here. The real memory
  * tests come first, because those are the most important to see.
  *
  * To do anything meaningful with memory, we must use
  * set and get expressions inside a statement block.
  */
object MemoryTests extends Properties("MemoryTests") {

  // helper variables we'll use a bunch here
  val (x,y,vx,vy) = ("x","y",v"x",v"y")

  // Memory tests!
  test((mem(0) := 10.n) mustBe (0, Map(0 -> 10)))
  test(block(mem(0) := 10.n) mustBe (0, Map(0 -> 10)))

  test(block(
    mem(0) := 5.n,
    mem(1) := 6.n,
    mem(0) + mem(1)
  ) mustBe (11, Map(0 -> 5, 1 -> 6)))

  // forall a,v: { mem(a) := v; mem(a) } = v
  test("set then get")(forAll {
    (addr: Int, value: Int) => block(
      mem(addr) := value.n, mem(addr)
    ) mustBe (value, Map(addr -> value))
  })

  // pointers!
  // forall a1,a2,v: {mem(a1) := a2; mem(a2) := v; mem(mem(a1))} = v
  test("pointers")(
    forAll { (a1: Int, a2: Int, value: Int) => a1 != a2 ==> {
      block(
        mem(a1) := a2.n, mem(a2) := value.n, mem(mem(a1))
      ) mustBe (value, Map(a1 -> a2, a2 -> value))
    }}
  )

  test("consecutive sets")(
    forAll { (a: Int, v1: Int, v2: Int) => v1 != v2 ==> {
      block(
        mem(a) := v1.n, mem(a) := v2.n, mem(a)
      ) mustBe (v2, Map(a -> v2))
    }}
  )

  // tests from FirstLang
  test(7.n mustBe (7, Map()))
  test(5.n + 6.n mustBe (11, Map()))
  test(5.n  * 6.n mustBe (30, Map()))
  test((5.n * 6.n) * (5.n + 5.n) mustBe (300, Map()))
  test((5.n + 6.n) + (5.n * 5.n) mustBe (36, Map()))

  // tests from let
  test(x -> 9.n in vx mustBe (9, Map()))
  test(x -> 9.n in vx * vx mustBe (81, Map()))
  test(x -> 9.n in (x ->  8.n in (vx * vx)) mustBe (64, Map()))
  test(x -> 9.n in (y ->  8.n in (x -> 7.n in vy * vx)) mustBe (56, Map()))

  /** simple statement blocks that dont use memory **/
  // this is the empty program
  test(block() mustBe (0, Map()))
  // statement block with let
  test(x -> 9.n in block(vx) mustBe (9, Map()))
  // useless values in block (here 0 and 5 accomplish nothing.
  // the scala code for this block would be: { 0; 5; 10 }
  test(block(0.n,5.n,10.n) mustBe (10, Map()))
  // nested useless blocks
  // scala: {{0; 5; 10}; {0; 5; 11}}
  test(block(block(0.n,5.n,10.n), block(0.n,5.n,11.n)) mustBe (11, Map()))


  def test(name:String)(f: => Prop): Unit = {
    property(name) = secure { f }
    ()
  }

  implicit def tProp(t: (Exp,Int,Mem)): Prop = testBody(t)

  def test(t: (Exp,Int,Mem)): Unit = {
    test(t._1.toString)(testBody(t))
  }

  def testBody(t: (Exp,Int,Mem)): Prop =
    secure {
      val exp: Exp = t._1
      val res: Int = t._2
      val mem: Mem = t._3
      eval(exp) == (res -> mem)
    }

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  implicit class RichExp(e:Exp) {
    def +(e2: Exp) = Add(e, e2)
    def *(e2: Exp) = Mult(e, e2)
    def mustBe(value: Int, mem: Map[Int, Int]) = (e, value, mem)
  }

  implicit class RichGetMem(e:GetMem) {
    def := (exp:Exp) = SetMem(e.address,exp)
  }

  def block(e:Exp*) = Statements(e.toList)

  implicit class RichWhat(t: (String, Exp)) {
    def in(e:Exp) = Let(t,e)
  }

  def mem(i: Int): GetMem = GetMem(i.n)
  def mem(e: Exp): GetMem = GetMem(e)

  implicit class RichInt(i:Int) {
    def n = Num(i)
  }
}

