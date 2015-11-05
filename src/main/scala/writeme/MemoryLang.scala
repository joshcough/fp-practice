package writeme

object MemoryLang {

  trait Exp
    case class Num(i:Int)                       extends Exp
    case class Add (l:Exp, r:Exp)               extends Exp
    case class Mult(l:Exp, r:Exp)               extends Exp
    case class Var (v: String)                  extends Exp
    case class Let (v: (String, Exp), body:Exp) extends Exp
    case class SetMem(address: Int, e: Exp)     extends Exp
    case class GetMem(address: Int)             extends Exp
    case class Statements(es:List[Exp])         extends Exp

  type Env = Map[String, Int]
  type Mem = Map[Int,    Int]

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  def lookup(v: String, env: Env, mem: Mem): Int =
    env.getOrElse(v, sys.error(s"unbound variable: $v, env: $env, mem: $mem"))

  def readMem(addr: Int, mem: Mem, env: Env): Int =
    mem.getOrElse(addr, sys.error(s"null pointer: $addr, env: $env, mem: $mem"))

  def interp(node: Exp,
             env: Env=Map(),
             mem: Mem=Map()): (Int, Mem) = node match {
    case Num (i)   => ???
    case Add (l,r) => ???
    case Mult(l,r) => ???
    case Var (x)   => ???
    case Let ((x,e),b) => ???
    case SetMem(address:Int, e:Exp) => ???
    case GetMem(addr:Int) => ???
    case Statements(es)   => ???
  }

  def run(node: Exp, expected: (Int, Mem)) = {
    val (i,m) = interp(node)
    println(i -> m)
    if((i,m)!=expected) sys.error(s"expected: $expected, but got: $i")
  }

  def main (args: Array[String]): Unit = {
    run(SetMem(0, n"10"), (10 ,Map(0 -> 10)))
    run(Statements(List(SetMem(0, n"10"), GetMem(0))), (10, Map(0 -> 10)))
    run(Statements(List(
      SetMem(0, n"10"),
      SetMem(0, n"15"),
      GetMem(0))), (15, Map(0 -> 15)))

    //
    //  {
    //    set(0 -> 10)
    //    set(1, { val x = 42 + get(0); set(2, x) })
    //    get(0)
    //  }
    //

    run(Statements(List(
      SetMem(0, n"10"),
      SetMem(1, Let(("x", Add(n"42", GetMem(0))), SetMem(2, v"x"))),
      GetMem(0))), (10, Map(0 -> 10, 1 -> 52, 2 -> 52)))
  }
}
