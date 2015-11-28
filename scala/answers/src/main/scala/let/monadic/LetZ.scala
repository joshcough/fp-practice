package let.monadic

import LetAST._

// With Reader Monad
object LetZ {

  import scalaz._
  import Scalaz._
  import scalaz.Kleisli._

  type Env = Map[String, Int]

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  type R[A] = Reader[Env, A]

  def lookup(v: String, env: Env): Int =
    env.getOrElse(v, sys.error(s"unbound variable: $v, env: $env"))

  def interp(exp: Exp): R[Int] = exp match {
    case Num (i)   => i.point[R]
    case Add (l,r) => for {
      lx  <- interp(l)
      rx  <- interp(r)
    } yield lx + rx
    case Mult(l,r) => for {
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
}
