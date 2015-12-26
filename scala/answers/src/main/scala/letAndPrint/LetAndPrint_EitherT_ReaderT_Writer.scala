package letAndPrint

import letAndPrint.LetAndPrint._

import scala.language.higherKinds
import scalaz.Kleisli._
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.monad._
import scalaz.syntax.monadError._
import scalaz.syntax.traverse._
import scalaz.syntax.writer._
import scalaz.{\/, EitherT, ReaderT, Writer}

/**
  * Example of MonadTransformers, EitherT/ReaderT/Writer
  */
object LetAndPrint_EitherT_ReaderT_Writer extends Interpreter {

  type W[A]       = Writer[Output,A]
  type Y[F[_],A]  = ReaderT[F,Env,A]
  type R[A]       = Y[W,A]
  type B[E,A]     = EitherT[R,E,A]
  type Z[F[_], A] = EitherT[F,String,A]
  type E[A]       = Z[R, A]

  def interpret(e:Exp): (Output, Int) = {
    val (o, v) = eval(e).run.run(Map()).run
    (o, v.fold(die(_, Map()), identity))
  }

  def eval(exp: Exp): E[Int]  =
    exp match {
      case Num (i)        => i.pure[E]
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
        env <- ask[W,Env].liftM[Z]: E[Env]
        res <- lookup(x,env)
      } yield res
      case Let ((x,e),b)  => for {
        env <- ask[W,Env].liftM[Z]: E[Env]
        ev  <- eval(e)
        bv  <- myLocal(x,ev,env,b)
      } yield bv
      case Print(e)       => for {
        ev <- eval(e)
        _  <- (ev.set(List(ev.toString)) : W[Int]).liftM[Y].liftM[Z]: E[Int]
      } yield ev
      case Statements(es) => es.foldlM(0)(_ => eval)
    }

  def lookup(v: String, env: Env): E[Int] =
    env.get(v).fold[B[String, Int]](
      s"unbound variable $v".raiseError[B,Int])((i: Int) => i.point[E])

  def myLocal(x:String, ev: Int, env: Env, b: Exp): E[Int] = for {
    v   <- local((env: Env) => env + (x -> ev))(eval(b).run).liftM[Z] : E[String\/Int]
    res <- v.fold[B[String, Int]](_.raiseError[B,Int], (i: Int) => i.point[E])
  } yield res
}
