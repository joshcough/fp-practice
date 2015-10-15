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
    mem.getOrElse(addr, sys.error(s"null pointer: $addr, env: $env"))

  def interp(node: Exp,
             env: Env=Map(),
             mem: Mem=Map()): (Int,Mem) = node match {
    case Num (i)   => (i,mem)
    case Add (l,r) =>
      val (il,meml) = interp(l,env, mem)
      val (ir,memr) = interp(r,env, meml)
      (il+ir, memr)
    case Mult(l,r) =>
      val (il,meml) = interp(l,env, mem)
      val (ir,memr) = interp(r,env, meml)
      (il*ir, memr)
    case Var (x)   => (lookup(x,env,mem), mem)
    case Let ((x,e),b) =>
      val (eValue,memx) = interp(e, env, mem)
      interp(b, env + (x -> eValue), memx)
    case SetMem(address:Int, e:Exp) =>
      val (eValue,memx) = interp(e, env, mem)
      // we can return any value here...
      (0, memx + (address -> eValue))
    case GetMem(addr:Int) => (readMem(addr, mem, env), mem)
    case Statements(es)   =>
      es.foldLeft((0,mem)){ case ((_,memacc),e) =>
        interp(e, env, memacc)
      }
  }

  def run(node: Exp, expected: Int) = {
    val (i,m) = interp(node)
    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
  }
}

