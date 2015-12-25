package let.monadic

import LetAST._

/**
  * Example of the Reader Monad
  */
object LetZ {

  import scalaz._
  import Scalaz._
  import scalaz.Kleisli._

  type Env = Map[String, Int]

  type R[A] = Reader[Env, A]

  def lookup(v: String, env: Env): Int =
    env.getOrElse(v, sys.error(s"unbound variable: $v, env: $env"))

  def eval(exp: Exp): R[Int] = exp match {
    case Num (i)   => i.point[R]
    case Add (l,r) => for {
      lx  <- eval(l)
      rx  <- eval(r)
    } yield lx + rx
    case Mult(l,r) => for {
      lx  <- eval(l)
      rx  <- eval(r)
    } yield lx * rx
    case Var (x)   => for { env <- ask[Id,Env] } yield lookup(x, env)
    case Let ((x,e),b) => for {
      env    <- ask[Id,Env]
      eValue <- eval(e)
      z      <- local((env: Env) => env + (x -> eValue))(eval(b))
    } yield z
  }
}
