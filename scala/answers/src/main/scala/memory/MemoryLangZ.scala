package memory

/**
  * Created by jcough on 11/29/15.
  */
object MemoryLangZ {

  import scalaz.State
  import scalaz.State._
  import scalaz.std.list._
  import scalaz.syntax.applicative._

  trait Exp
    case class Num(i:Int)                       extends Exp
    case class Add (l:Exp, r:Exp)               extends Exp
    case class Mult(l:Exp, r:Exp)               extends Exp
    case class Var (v: String)                  extends Exp
    case class Let (v: (String, Exp), body:Exp) extends Exp
    case class SetMem(address: Exp, e: Exp)     extends Exp
    case class GetMem(address: Exp)             extends Exp
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

  def interp(exp: Exp, env: Env=Map()): MemState[Int] = exp match {
    case Num (i)   => i.point[MemState]
    case Add (l,r) => for {
      il <- interp(l, env)
      ir <- interp(r, env)
    } yield il + ir
    case Mult (l,r) => for {
      il <- interp(l, env)
      ir <- interp(r, env)
    } yield il * ir
    case Var (x)   => lookup(x,env).point[MemState]
    case Let ((x,e),b) => for {
      ev <- interp(e, env)
      bv <- interp(b, env + (x -> ev))
    } yield bv
    case SetMem(address, e) => for {
      addr <- interp(address)
      v    <- interp(e)
      _    <- modify[Mem](s => s + (addr -> v))
    } yield 0
    case GetMem(address) => for {
      addr <- interp(address)
      m    <- get[Mem]
    } yield readMem(addr, m, env)
  }
}

