package let.monadic

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

  def interp(exp: Exp): R[Int] = exp match {
    case Num (i)       => ???
    case Add (l,r)     => ???
    case Mult(l,r)     => ???
    case Var (x)       => ???
    case Let ((x,e),b) => ???
  }
}
