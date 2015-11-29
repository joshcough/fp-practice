package memory

/**
  * As the name implies, MemoryLang has memory.
  * More accurately, it has a magical heap that stores
  * integer values at integer addresses.
  *
  * Similar to PrintSdtOut (using side-effects on print calls),
  * we could use a mutable map to store these values, and then
  * refactor the program to use an immutable map so that we can
  * see the benefits of pure functions with testing. But, we
  * already did that, so we are just going to skip that part.
  *
  * Here, you are asked to implement an extension of LetLang that adds:
  *  - set: set an address in memory to a value
  *  - get: get the value at a particular address in memory
  *  - statement blocks: execute n expressions linearly.
  *
  * The semantics of set and statement blocks are up to you.
  * For example, the statement { mem(0) = 100 } could return
  * several possible values:
  *  - the old value at that address
  *  - the new value to put into that address
  *  - the address
  *  - 0, -1, some other int value.
  */
object MemoryLang {

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

  def lookup(v: String, env: Env, mem: Mem): Int =
    env.getOrElse(v, sys.error(s"unbound variable: $v, env: $env, mem: $mem"))

  def readMem(addr: Int, mem: Mem, env: Env): Int =
    mem.getOrElse(addr, sys.error(s"null pointer: $addr, env: $env"))

  def interp(exp: Exp, env: Env=Map(),
             mem: Mem=Map()): (Int,Mem) = exp match {
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
    case SetMem(address: Exp, e:Exp) =>
      val (addrValue,memA) = interp(address, env, mem)
      val (eValue,memE) = interp(e, env, memA)
      // we can return any value here...
      // i chose to have set return 0 always.
      (0, memE + (addrValue -> eValue))
    case GetMem(address: Exp) =>
      val (addrValue,memA) = interp(address, env, mem)
      (readMem(addrValue, mem, env), memA)
    case Statements(es)    =>
      es.foldLeft((0,mem)){ case ((_,memacc),e) =>
        interp(e, env, memacc)
      }
  }

  def run(exp: Exp, expected: Int) = {
    val (i,m) = interp(exp)
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
//    case SetMem(address:Int, e:Exp) => for {
//      mem <- get
//    }


    //    case SetMem(address:Int, e:Exp) =>
//      val (eValue,memx) = interp(e, env, mem)
//      // we can return any value here...
//      (0, memx + (address -> eValue))
//    case GetMem(addr:Int) => (readMem(addr, mem, env), mem)
//    case Statements(es)   =>
//      es.foldLeft((0,mem)){ case ((_,memacc),e) =>
//        interp(e, env, memacc)
//      }
  }

//  def run(exp: Exp, expected: Int) = {
//    val (i,m) = interp(exp)
//    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
//  }
}


