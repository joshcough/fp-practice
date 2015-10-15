package answers

// With Writer Monad
object LetAndPrintZLang {

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
