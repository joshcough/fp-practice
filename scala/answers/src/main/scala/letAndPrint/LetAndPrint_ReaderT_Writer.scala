package letAndPrint

import LetAndPrint._
import scala.language.higherKinds
import scalaz.Kleisli._
import scalaz.{ReaderT, Writer}
import scalaz.std.list._
import scalaz.syntax.monad._
import scalaz.syntax.traverse._
import scalaz.syntax.writer._

/**
  * Example of MonadTransformers, ReaderT/Writer
  */
object LetAndPrint_ReaderT_Writer extends Interpreter {

  type W[A]       = Writer[Output, A]
  type Z[F[_], A] = ReaderT[F, Env, A]
  type R[A]       = Z[W, A]

  def interpret(e:Exp): (Output, Int) = eval(e).run(Map()).run

  def lookup(v: String, env: Env): R[Int] =
    env.getOrElse(v, die(s"unbound variable: $v", env)).pure[W].liftM[Z]

  def eval(exp: Exp): R[Int]  =
    exp match {
      case Num (i)        => i.pure[R]
      case Add (l,r)      => for {
        lv <- eval(l)
        rv <- eval(r)
      } yield lv+rv
      case Mult(l,r)      => for {
        lv <- eval(l)
        rv <- eval(r)
      } yield lv*rv
      case Eq  (l,r) => for {
        lv <- eval(l)
        rv <- eval(r)
      } yield if (lv == rv) 1 else 0
      case If(predicate,tBranch,fBranch) => for {
        pv  <- eval(predicate)
        res <- eval(if (pv == 1) tBranch else fBranch)
      } yield res
      case Var (x)        => for {
        env <- ask[W, Env]
        res <- lookup(x, env)
      } yield res
      case Let ((x,e),b)  => for {
        env <- ask[W,Env]
        ev  <- eval(e)
        bv  <- local((env: Env) => env + (x -> ev))(eval(b))
      } yield bv
      case Print(e)       => for {
        ev <- eval(e)
        _  <- ev.set(List(ev.toString)).liftM[Z] // TODO: I hate this.
      } yield ev
      case Statements(es) => es.foldlM(0)(_ => eval)
    }
}
