package answers

object Arith {

  trait Exp
  case class Num(i:Int)         extends Exp
  case class Add (l:Exp, r:Exp) extends Exp
  case class Mult(l:Exp, r:Exp) extends Exp

  def interp(node: Exp): Int = node match {
    case Num (i)   => i
    case Add (l,r) => interp(l) + interp(r)
    case Mult(l,r) => interp(l) * interp(r)
  }

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  def run(node: Exp, expected: Int) = {
    val i = interp(node)
    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
  }

  def main(args:Array[String]): Unit = {
    run(n"7", 7)
    run(Add (n"5", n"6"), 11)
    run(Mult(n"5", n"6"), 30)
    println("success!!")
  }
}

// Before Reader Monad
object Let {

  trait Exp
  case class Num(i:Int)                       extends Exp
  case class Add (l:Exp, r:Exp)               extends Exp
  case class Mult(l:Exp, r:Exp)               extends Exp
  case class Var (v: String)                  extends Exp
  case class Let (v: (String, Exp), body:Exp) extends Exp

  type Env = Map[String, Int]

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  def lookup(v: String, env: Env): Int =
    env.getOrElse(v, sys.error(s"unbound variable: $v, env: $env"))

  def interp(node: Exp, env: Env=Map()): Int = node match {
    case Num (i)   => i
    case Add (l,r) => interp(l,env) + interp(r,env)
    case Mult(l,r) => interp(l,env) * interp(r,env)
    case Var (x)   => lookup(x, env)
    case Let ((x,e),b) =>
      val eValue = interp(e, env)
      interp(b, env + (x -> eValue))
  }

  def run(node: Exp, expected: Int) = {
    val i = interp(node)
    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
  }

  def main(args:Array[String]): Unit = {
    run(n"7", 7)
    run(Add (n"5", n"6"), 11)
    run(Mult(n"5", n"6"), 30)
    run(Let(("x",  n"5"), v"x"), 5)
    run(Let(("x",  n"9"), Mult(v"x", v"x")), 81)
    println("success!!")
  }
}

// With Reader Monad
object LetZ {

  import scalaz._
  import Scalaz._
  import scalaz.Kleisli._

  trait Exp
  case class Num(i:Int)                       extends Exp
  case class Add (l:Exp, r:Exp)               extends Exp
  case class Mult(l:Exp, r:Exp)               extends Exp
  case class Var (v: String)                  extends Exp
  case class Let (v: (String, Exp), body:Exp) extends Exp

  type Env = Map[String, Int]

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  type R[A] = Reader[Env, A]

  def lookup(v: String, env: Env): Int =
    env.getOrElse(v, sys.error(s"unbound variable: $v, env: $env"))

  def interp(node: Exp): R[Int] = node match {
    case Num (i)   => i.point[R]
    case Add (l,r) => for {
      env <- ask[Id,Env]
      lx  <- interp(l)
      rx  <- interp(r)
    } yield lx + rx
    case Mult(l,r) => for {
      env <- ask[Id,Env]
      lx  <- interp(l)
      rx  <- interp(r)
    } yield lx * rx
    case Var (x)   => for { env <- ask[Id,Env] } yield lookup(x, env)
    case Let ((x,e),b) => for {
      env    <- ask[Id,Env]
      eValue <- interp(e)
      z      <- local((env: Env) => env + (x -> eValue))(interp(b))
    } yield z
  }

  def run(node: Exp, expected: Int) = {
    val i = interp(node)(Map())
    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
  }

  def main(args:Array[String]): Unit = {
    run(n"7", 7)
    run(Add (n"5", n"6"), 11)
    run(Mult(n"5", n"6"), 30)
    run(Let(("x",  n"5"), v"x"), 5)
    run(Let(("x",  n"9"), Mult(v"x", v"x")), 81)
    println("success!!")
  }
}

// Before Writer Monad
object Print {

  trait Exp
  case class Num  (i:Int)                      extends Exp
  case class Add  (l:Exp, r:Exp)               extends Exp
  case class Mult (l:Exp, r:Exp)               extends Exp
  case class Print(e: Exp)                     extends Exp
  case class Statements(es:List[Exp])          extends Exp

  type Env    = Map[String, Int]
  type Output = List[String]

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  def interp(node: Exp,
             env: Env=Map(),
             output: Output=List()): (Output, Int)  =
    node match {
      case Num (i)        => (output, i)
      case Add (l,r)      =>
        val (lo,lv) = interp(l,env,output)
        val (ro,rv) = interp(r,env,lo)
        (ro,lv+rv)
      case Mult(l,r)      =>
        val (lo,lv) = interp(l,env,output)
        val (ro,rv) = interp(r,env,lo)
        (ro,lv*rv)
      case Print(e)       =>
        val (nextOutput,eValue) = interp(e, env, output)
        (nextOutput ++ List(eValue.toString), 0)
      case Statements(es) =>
        es.foldLeft((output,0)){ case ((outacc,_),e) =>
          interp(e, env, outacc)
        }
    }

  def run(node: Exp, expected: Int) = {
    val (output,i) = interp(node)
    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
  }

  def main(args:Array[String]): Unit = {
    run(n"7", 7)
    run(Add (n"5", n"6"), 11)
    run(Mult(n"5", n"6"), 30)
    println("success!!")
  }
}

// With Writer Monad
object PrintZ {

  import scalaz.Writer
  import scalaz.std.list._
  import scalaz.syntax.writer._
  import scalaz.syntax.applicative._
  import scalaz.syntax.foldable._

  trait Exp
  case class Num  (i:Int)                      extends Exp
  case class Add  (l:Exp, r:Exp)               extends Exp
  case class Mult (l:Exp, r:Exp)               extends Exp
  case class Print(e: Exp)                     extends Exp
  case class Statements(es:List[Exp])          extends Exp

  type Env    = Map[String, Int]
  type Output = List[String]

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  type W[A] = Writer[Output, A]

  def die[A](msg: String, env: Env): A = sys.error(s"error: $msg, env: $env")

  def lookup(v: String, env: Env): W[Int] =
    env.getOrElse(v, die(s"unbound variable: $v", env)).pure[W]

  def interp(node: Exp, env: Env=Map()): W[Int]  =
    node match {
      case Num (i)        => i.pure[W]
      case Add (l,r)      => for {
        lv <- interp(l,env)
        rv <- interp(l,env)
      } yield lv+rv
      case Mult(l,r)      => for {
        lv <- interp(l,env)
        rv <- interp(l,env)
      } yield lv*rv
      case Print(e)       => for {
        ev <- interp(e, env)
        _  <- 0.set(List(ev.toString))
      } yield 0
      case Statements(es) => es.foldlM(0)(_ => e => interp(e, env))
    }

  def run(node: Exp, expected: Int) = {
    val (out,i) = interp(node).run
    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
  }

  def main(args:Array[String]): Unit = {
    run(n"7", 7)
    run(Add (n"5", n"6"), 11)
    run(Mult(n"5", n"6"), 30)
    println("success!!")
  }
}

// Before Writer Monad
object LetAndPrint {

  trait Exp
  case class Num  (i:Int)                      extends Exp
  case class Add  (l:Exp, r:Exp)               extends Exp
  case class Mult (l:Exp, r:Exp)               extends Exp
  case class Var  (v: String)                  extends Exp
  case class Let  (v: (String, Exp), body:Exp) extends Exp
  case class Print(e: Exp)                     extends Exp
  case class Statements(es:List[Exp])          extends Exp

  type Env    = Map[String, Int]
  type Output = List[String]

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  def die[A](msg: String, env: Env, out: Output): A =
    sys.error(s"error: $msg, env: $env, output: ${out.mkString("\n")}")

  def lookup(v: String, env: Env, output: Output): Int =
    env.getOrElse(v, die(s"unbound variable: $v", env, output))

  def interp(node: Exp,
             env: Env=Map(),
             output: Output=List()): (Output, Int)  =
    node match {
      case Num (i)        => (output, i)
      case Add (l,r)      =>
        val (lo,lv) = interp(l,env,output)
        val (ro,rv) = interp(r,env,lo)
        (ro,lv+rv)
      case Mult(l,r)      =>
        val (lo,lv) = interp(l,env,output)
        val (ro,rv) = interp(r,env,lo)
        (ro,lv*rv)
      case Var (x)        => (output, lookup(x, env, output))
      case Let ((x,e),b)  =>
        val (nextOutput,eValue) = interp(e, env, output)
        interp(b, env + (x -> eValue), nextOutput)
      case Print(e)       =>
        val (nextOutput,eValue) = interp(e, env, output)
        (nextOutput ++ List(eValue.toString), 0)
      case Statements(es) =>
        es.foldLeft((output,0)){ case ((outacc,_),e) =>
          interp(e, env, outacc)
        }
    }

  def run(node: Exp, expected: Int) = {
    val (output,i) = interp(node)
    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
  }

  def main(args:Array[String]): Unit = {
    run(n"7", 7)
    run(Add (n"5", n"6"), 11)
    run(Mult(n"5", n"6"), 30)
    run(Let(("x",  n"5"), v"x"), 5)
    run(Let(("x",  n"9"), Mult(v"x", v"x")), 81)
    println("success!!")
  }
}

// With Writer Monad
object LetAndPrintZ {

  import scalaz.Writer
  import scalaz.std.list._
  import scalaz.syntax.writer._
  import scalaz.syntax.applicative._
  import scalaz.syntax.foldable._

  trait Exp
  case class Num  (i:Int)                      extends Exp
  case class Add  (l:Exp, r:Exp)               extends Exp
  case class Mult (l:Exp, r:Exp)               extends Exp
  case class Var  (v: String)                  extends Exp
  case class Let  (v: (String, Exp), body:Exp) extends Exp
  case class Print(e: Exp)                     extends Exp
  case class Statements(es:List[Exp])          extends Exp

  type Env    = Map[String, Int]
  type Output = List[String]

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  type W[A] = Writer[Output, A]

  def die[A](msg: String, env: Env): A = sys.error(s"error: $msg, env: $env")

  def lookup(v: String, env: Env): W[Int] =
    env.getOrElse(v, die(s"unbound variable: $v", env)).pure[W]

  def interp(node: Exp, env: Env=Map()): W[Int]  =
    node match {
      case Num (i)        => i.pure[W]
      case Add (l,r)      => for {
        lv <- interp(l,env)
        rv <- interp(l,env)
      } yield lv+rv
      case Mult(l,r)      => for {
        lv <- interp(l,env)
        rv <- interp(l,env)
      } yield lv*rv
      case Var (x)        => lookup(x, env)
      case Let ((x,e),b)  => for {
        ev <- interp(e, env)
        bv <- interp(b, env + (x -> ev))
      } yield bv
      case Print(e)       => for {
        ev <- interp(e, env)
        _  <- 0.set(List(ev.toString))
      } yield 0
      case Statements(es) => es.foldlM(0)(_ => e => interp(e, env))
    }

  def run(node: Exp, expected: Int) = {
    val (out,i) = interp(node).run
    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
  }

  def main(args:Array[String]): Unit = {
    run(n"7", 7)
    run(Add (n"5", n"6"), 11)
    run(Mult(n"5", n"6"), 30)
    run(Let(("x",  n"5"), v"x"), 5)
    run(Let(("x",  n"9"), Mult(v"x", v"x")), 81)
    println("success!!")
  }
}


// With Writer Monad
object LetAndPrintTransformers {

  import scalaz.Writer
  import scalaz.std.list._
  import scalaz.syntax.writer._
  import scalaz.syntax.applicative._
  import scalaz.syntax.foldable._

  trait Exp
  case class Num  (i:Int)                      extends Exp
  case class Add  (l:Exp, r:Exp)               extends Exp
  case class Mult (l:Exp, r:Exp)               extends Exp
  case class Var  (v: String)                  extends Exp
  case class Let  (v: (String, Exp), body:Exp) extends Exp
  case class Print(e: Exp)                     extends Exp
  case class Statements(es:List[Exp])          extends Exp

  type Env    = Map[String, Int]
  type Output = List[String]

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  type W[A] = Writer[Output, A]

  def die[A](msg: String, env: Env): A = sys.error(s"error: $msg, env: $env")

  def lookup(v: String, env: Env): W[Int] =
    env.getOrElse(v, die(s"unbound variable: $v", env)).pure[W]

  def interp(node: Exp, env: Env=Map()): W[Int]  =
    node match {
      case Num (i)        => i.pure[W]
      case Add (l,r)      => for {
        lv <- interp(l,env)
        rv <- interp(l,env)
      } yield lv+rv
      case Mult(l,r)      => for {
        lv <- interp(l,env)
        rv <- interp(l,env)
      } yield lv*rv
      case Var (x)        => lookup(x, env)
      case Let ((x,e),b)  => for {
        ev <- interp(e, env)
        bv <- interp(b, env + (x -> ev))
      } yield bv
      case Print(e)       => for {
        ev <- interp(e, env)
        _  <- 0.set(List(ev.toString))
      } yield 0
      case Statements(es) => es.foldlM(0)(_ => e => interp(e, env))
    }

  def run(node: Exp, expected: Int) = {
    val (out,i) = interp(node).run
    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
  }

  def main(args:Array[String]): Unit = {
    run(n"7", 7)
    run(Add (n"5", n"6"), 11)
    run(Mult(n"5", n"6"), 30)
    run(Let(("x",  n"5"), v"x"), 5)
    run(Let(("x",  n"9"), Mult(v"x", v"x")), 81)
    println("success!!")
  }
}


object Memory {

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

  def main(args:Array[String]): Unit = {
    run(n"7", 7)
    run(Add (n"5", n"6"), 11)
    run(Mult(n"5", n"6"), 30)
    run(Let(("x",  n"5"), v"x"), 5)
    run(Let(("x",  n"9"), Mult(v"x", v"x")), 81)
    println(interp(SetMem(0, n"10")))
    println("success!!")
  }
}

