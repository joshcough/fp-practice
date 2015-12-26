package letAndPrint

import letAndPrint.LetAndPrint._

import scala.language.higherKinds
import scalaz.Kleisli._
import scalaz.State._
import scalaz.WriterT._
import scalaz.std.list._
import scalaz.syntax.monad._
import scalaz.syntax.monadError._
import scalaz.syntax.traverse._
import scalaz.syntax.writer._
import scalaz.{WriterT, State, EitherT, ReaderT, \/}

/**
  * Example of MonadTransformers, EitherT/ReaderT/WriterT/State
  */
object LetAndPrint_EitherT_ReaderT_WriterT_State extends Interpreter {

  type Mem = Map[Int, Int]

  case class SetMem(address: Exp, e: Exp) extends Exp
  case class GetMem(address: Exp)         extends Exp

  type S[A]       = State[Mem, A]
  type U[F[_], A] = WriterT[F, Output, A]
  type W[A]       = U[S, A]
  type Y[F[_],A]  = ReaderT[F,Env,A]
  type R[A]       = Y[W,A]
  type B[E,A]     = EitherT[R,E,A]
  type Z[F[_], A] = EitherT[F,String,A]
  type E[A]       = Z[R, A]

  def interpret(e:Exp): (Output, Int) = {
    val (m, (o, v)) = eval(e).run.run(Map()).run.run(Map())
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
        res <- lookup(s"unbound variable: $x")(x,env)
      } yield res
      case Let ((x,e),b)  => for {
        env <- ask[W,Env].liftM[Z]: E[Env]
        ev  <- eval(e)
        bv  <- localE(x,ev,env,b)
      } yield bv
      case Print(e) => for {
        ev <- eval(e)
        _  <- writerE(writerT((List(ev.toString), ev).point[S]))
      } yield ev
      case SetMem(address, e) => for {
        mem  <- stateE(get[Mem])
        addr <- eval(address)
        ev   <- eval(e)
        _    <- stateE(State.put(mem + (addr -> ev)))
      } yield 0
      case GetMem(address)    => for {
        mem  <- stateE(get[Mem])
        addr <- eval(address)
        res  <- lookup(s"null pointer: $addr")(addr,mem)
      } yield res
      case Statements(es) => es.foldlM(0)(_ => eval)
    }

  def writerE[A](w:W[A]): E[A] = w.liftM[Y].liftM[Z]
  def stateE [A](s:S[A]): E[A] = writerE(s.liftM[U])

  def lookup[A](msg: => String)(v: A, m: Map[A,Int]): E[Int] =
    m.get(v).fold[B[String, Int]](
      s"$msg: $v".raiseError[B,Int])((i: Int) => i.point[E])

  def localE(x:String, ev: Int, env: Env, b: Exp): E[Int] = for {
    v   <- local((env: Env) => env + (x -> ev))(eval(b).run).liftM[Z] : E[String\/Int]
    res <- v.fold[B[String, Int]](_.raiseError[B,Int], (i: Int) => i.point[E])
  } yield res
}
