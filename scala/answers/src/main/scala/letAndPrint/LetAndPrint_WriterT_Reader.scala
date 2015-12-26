package letAndPrint

import LetAndPrint._

import scalaz.{Reader, WriterT}
import scalaz.WriterT._
import scalaz.Kleisli.{ask, local}
import scalaz.Id.Id
import scalaz.std.list._
import scalaz.syntax.monad._
import scalaz.syntax.traverse._
import scalaz.syntax.writer._
import scalaz.syntax.kleisli._

import scala.language.higherKinds

/**
  * Example of MonadTransformers, WriterT/Reader
  */
object LetAndPrint_WriterT_Reader extends Interpreter {

  type R[A]       = Reader[Env, A]
  type Z[F[_], A] = WriterT[F, Output, A]
  type W[A]       = Z[R, A]

  def interpret(e:Exp): (Output, Int) = eval(e).run(Map())

  def lookup(v: String, env: Env): W[Int] =
    env.getOrElse(v, die(s"unbound variable: $v", env)).pure[R].liftM[Z]

  def eval(exp: Exp): W[Int]  =
    exp match {
      case Num (i)   => i.pure[W]
      case Add (l,r) => for {
        lv <- eval(l)
        rv <- eval(r)
      } yield lv+rv
      case Mult(l,r) => for {
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
        env <- ask[Id,Env].liftM[Z]
        res <- lookup(x, env)
      } yield res
      case Let ((x,e),b)  => for {
        env <- ask[Id,Env].liftM[Z]
        ev  <- eval(e)
        bv  <- writerT[R,Output,Int](
          local((env: Env) => env + (x -> ev))(eval(b).run)
        )
      } yield bv
      case Print(e) => for {
        ev <- eval(e)
        _ <- writerT((List(ev.toString), ev).point[R])
      } yield ev
      case Statements(es) => es.foldlM(0)(_ => eval)
    }
}
