package memory

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
    mem.getOrElse(addr, sys.error(s"null pointer: $addr, env: $env"))

  def interp(node: Exp,
             env: Env=Map(),
             mem: Mem=Map()): (Int,Mem) = node match {
    case Num (i)          => ???
    case Add (l,r)        => ???
    case Mult(l,r)        => ???
    case Var (x)          => ???
    case Let ((x,e),b)    => ???
    case Statements(es)   => ???
    case GetMem(addr:Int) => ???
    case SetMem(address:Int, e:Exp) => ???
  }

  def run(node: Exp, expected: Int) = {
    val (i,m) = interp(node)
    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
  }
}

object MemoryLangZ {

  import scalaz.State
  import scalaz.std.list._
  import scalaz.syntax.applicative._

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

  type MemState[A] = State[Mem, A]

  def lookup(v: String, env: Env): Int =
    env.getOrElse(v, sys.error(s"unbound variable: $v, env: $env"))

  def readMem(addr: Int, mem: Mem, env: Env): Int =
    mem.getOrElse(addr, sys.error(s"null pointer: $addr, env: $env"))

  def interp(node: Exp, env: Env=Map()): MemState[Int] = node match {
    case Num (i)       => ???
    case Add (l,r)     => ???
    case Mult (l,r)    => ???
    case Var (x)       => ???
    case Let ((x,e),b) => ???

  }
}


