package writeme

/**
 * Implement this expression 'language'.
 * It might be the easiest thing ever.
 */
object Arith {

  trait Exp
  case class Num(i:Int)         extends Exp
  case class Add (l:Exp, r:Exp) extends Exp
  case class Mult(l:Exp, r:Exp) extends Exp

  def interp(node: Exp): Int = node match {
    case Num (i)   => ???
    case Add (l,r) => ???
    case Mult(l,r) => ???
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

/**
 * The previous Arith language with Let.
 * (can't use Reader Monad)
 */
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
    case Num (i)       => ???
    case Add (l,r)     => ???
    case Mult(l,r)     => ???
    case Var (x)       => ???
    case Let ((x,e),b) => ???
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

/**
 * The previous Arith language with Let.
 * (must use Reader Monad)
 */
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
    case Num (i)       => ???
    case Add (l,r)     => ???
    case Mult(l,r)     => ???
    case Var (x)       => ???
    case Let ((x,e),b) => ???
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

/**
 * The previous Arith language with Print.
 * (can't use Writer Monad)
 */
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
             output: Output=List()): (Output, Int)  =
    node match {
      case Num (i)        => ???
      case Add (l,r)      => ???
      case Mult(l,r)      => ???
      case Print(e)       => ???
      case Statements(es) => ???
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

/**
 * The previous Arith language with Print.
 * (must use Writer Monad)
 */
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
      case Num (i)        => ???
      case Mult(l,r)      => ???
      case Print(e)       => ???
      case Statements(es) => ???
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
      case Num (i)        => ???
      case Add (l,r)      => ???
      case Mult(l,r)      => ???
      case Var (x)        => ???
      case Let ((x,e),b)  => ???
      case Print(e)       => ???
      case Statements(es) => ???
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
      case Num (i)        => ???
      case Mult(l,r)      => ???
      case Var (x)        => ???
      case Let ((x,e),b)  => ???
      case Print(e)       => ???
      case Statements(es) => ???
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

/**
 * Challenge: write a mutable programming language
 * without(!) using any mutable vars or mutable data structures.
 */
object Memory {

  trait Exp
  case class Num(i:Int)                       extends Exp
  case class Add (l:Exp, r:Exp)               extends Exp
  case class Mult(l:Exp, r:Exp)               extends Exp
  case class Var (v: String)                  extends Exp
  case class Let (v: (String, Exp), body:Exp) extends Exp
  case class Statements(es:List[Exp])         extends Exp
  case class SetMem(address: Int, e: Exp)     extends Exp
  case class GetMem(address: Int)             extends Exp

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
    case Num (i)                 => ???
    case Add (l,r)               => ???
    case Mult(l,r)               => ???
    case Var (x)                 => ???
    case Let ((x,e),b)           => ???
    case SetMem(addr:Int, e:Exp) => ???
    case GetMem(addr:Int)        => ???
    case Statements(es)          => ???
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

