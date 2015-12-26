package letAndPrint

import letAndPrint.LetAndPrint._

import scala.language.higherKinds
import scalaz.{EitherT, ReaderWriterState, \/}
import scalaz.std.list._
import scalaz.syntax.either._
import scalaz.syntax.monad._
import scalaz.syntax.traverse._

/**
  * Example of MonadTransformers, ReaderWriterState
  */
object LetAndPrint_EitherT_ReaderWriterState extends Interpreter {

  type Mem = Map[Int, Int]

  case class SetMem(address: Exp, e: Exp) extends Exp
  case class GetMem(address: Exp)         extends Exp

  type RE[A]      = ReaderWriterState[Env,Output,Mem,String \/ A]
  type R[A]       = ReaderWriterState[Env,Output,Mem,A]
  type B[E,A]     = EitherT[R,E,A]
  type Z[F[_], A] = EitherT[F,String,A]
  type E[A]       = Z[R, A]

  def interpret(e:Exp): (Output, Int) = {
    val (o,v,m) = eval(e).run.run(Map(), Map())
    (o, v.fold(die(_, Map()), identity))
  }

  def eval(exp: Exp): E[Int]  =
    exp match {
      case Num (i)    => i.pure[E]
      case Add (l,r)  => for {
        lv <- eval(l)
        rv <- eval(r)
      } yield lv+rv
      case Mult(l,r)  => for {
        lv <- eval(l)
        rv <- eval(r)
      } yield lv*rv
      case Eq  (l,r)  => for {
        lv <- eval(l)
        rv <- eval(r)
      } yield if (lv == rv) 1 else 0
      case If(predicate,tBranch,fBranch) => for {
        pv  <- eval(predicate)
        res <- eval(if (pv == 1) tBranch else fBranch)
      } yield res
      case Var (x) => lookupEnv(s"unbound variable: $x")(x).e
      case Let ((x,e),b)  => for {
        ev  <- eval(e)
        bv  <- localE(x,ev,b).e
      } yield bv
      case Print(e) => for {
        ev <- eval(e)
        _ <- print(ev).e
      } yield ev
      case SetMem(address, e) => for {
        addr <- eval(address)
        ev   <- eval(e)
        _    <- setMem(addr, ev).e
      } yield 0
      case GetMem(address) => for {
        addr <- eval(address)
        res  <- lookupMem(s"null pointer: $addr")(addr).e
      } yield addr
      case Statements(es) => es.foldlM(0)(_ => eval)
    }

  implicit class RichRWSV[A](rws: RE[A]) {
    def e: E[A] = EitherT[R, String, A](rws)
  }

  implicit class RichRWS[A](rws: R[A]) {
    def e: E[A] = EitherT[R, String, A](rws.map(_.right))
  }

  def print(i: Int): RE[Unit] = ReaderWriterState {
    (_:Env, s:Mem) => (List(i.toString), ().right, s)
  }

  def withEnv[A](f: Env => String \/ A): RE[A] =
    ReaderWriterState { (env, mem) => (List(), f(env), mem) }

  def withMem[A](f: Mem => String \/ A): RE[A] =
    ReaderWriterState { (_, mem) => (List(), f(mem), mem) }

  def lookupEnv[A](msg: => String)(v: String): RE[Int] =
    withEnv(_.get(v).fold[String \/ Int](msg.left)(_.right))

  def lookupMem[A](msg: => String)(v: Int): RE[Int] =
    withMem(_.get(v).fold[String \/ Int](msg.left)(_.right))

  def setMem[A](addr: Int, newVal: Int): R[Unit] =
    ReaderWriterState { (_, mem) =>
      (List(), ().right, mem + (addr -> newVal))
    }

  def localE(x:String, ev: Int, b: Exp): RE[Int] =
    ReaderWriterState { (env, mem) =>
      eval(b).run.run(env + (x -> ev), mem)
    }
}
