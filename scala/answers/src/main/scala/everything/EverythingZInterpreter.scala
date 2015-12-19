package everything

import Everything._
import scala.language.higherKinds
import scalaz.syntax.either._
import scalaz.syntax.monad._
import scalaz.State
import scalaz.State._
import scalaz.EitherT
import scalaz.\/
import scalaz.syntax.traverse._
import scalaz.std.list._

/**
[20:54:18]  <edwardk>	EitherT e (State s)
[20:55:17]  <edwardk>	running that with an input state gives you back a pair of Either e a  and a state
[20:55:43]  <joshcough>	EitherT  gives you back a pair?
[20:55:44]  <edwardk>	on the other hand StateT s (Either e) will give back back Either e (a, s)
[20:56:18]  <edwardk>	so EitherT e (State s) a ~ s -> (Either e a, s)    while StateT s (Either e) a ~ s -> Either e (a, s)
  */
object EverythingZInterpreter extends Interpreter {

  override def interpret(exp: Exp): (String \/ RuntimeValue, ProgramState) =
    interp(exp).run.run(ProgramState()).swap

  type S[A] = State[ProgramState, A]
  type E[A] = EitherT[S, String, A]

  def interp(exp: Exp): E[RuntimeValue] =
    exp match {
      case Num (i)        => numV_E(i)
      case Add (l,r)      => mathInterp(l, r)(_+_)
      case Mult(l,r)      => mathInterp(l, r)(_*_)
      case Var (x)        => lookup(x)
      case Let ((x,e),b)  => interp(Apply(Function(x, b), e))
      case Print(e)       => for { ev <- interp(e); _ <- print(ev) } yield ev
      case Apply(fexp, a) => for {
        c   <- interpClosure(fexp)
        v   <- interp(a)
        res <- localE(_ => c.env + (c.f.arg -> v))(interp(c.f.body))
      } yield res
      case f:Function     => lift(get[ProgramState].map(p => closure(f, p.env)))
      case SetMem(address, e) => for {
        addr   <- interp(address)
        newVal <- interp(e)
        res    <- withAddress(addr)(writeMem(_, newVal))
      } yield numV(0)
      case GetMem(address) => for {
        addr <- interp(address)
        res  <- withAddress(addr)(readMem)
      } yield res
      case Statements(es) =>
        // for empty statement blocks, we just return 0.
        es.traverse(interp).map(_.lastOption.getOrElse(numV(0)))
    }

  def liftV[A](s:S[String \/ A]): E[A] = EitherT[S, String, A](s)
  def lift[A](s:S[A]): E[A] = liftV(s.map(_.right))

  def usingState[A](lens: ProgramState => Option[RuntimeValue],
                    failure: ProgramState => String): E[RuntimeValue] =
    liftV(get[ProgramState].map(s =>
      lens(s) match { case Some(r) => r.right; case _ => failure(s).left }
    ))

  def localE[A](f: Env => Env)(action: E[RuntimeValue]): E[RuntimeValue] =
    liftV(localS(f)(action.run))

  def localS[A](f: Env => Env)(action: => S[A]): S[A] =
    for {
      old <- get[ProgramState]
      _   <- put[ProgramState](old.copy(env = f(old.env)))
      res <- action
      _   <- modify[ProgramState](_.copy(env = old.env))
    } yield res

  def lookup(v: String): E[RuntimeValue] =
    usingState(_.env.get(v), s => s"unbound variable: $v in $s")

  def readMem(addr: Int): E[RuntimeValue] =
    usingState(_.mem.get(addr), s => s"null pointer: $addr in $s")

  def mod(f: ProgramState => ProgramState): E[Unit] =
    lift(modify[ProgramState](f))

  def writeMem(addr: Int, rv: RuntimeValue): E[Unit] =
    mod(ps => ps.copy(mem = ps.mem + (addr -> rv)))

  def print(rv: RuntimeValue): E[Unit] =
    mod(ps => ps.copy(out = ps.out ++ List(rv.toString)))

  def withAddress[A](addr: RuntimeValue)(f: Int => E[A]): E[A] =
    addr.fold(f(_), (_,_) => err("<function> is not an address"))

  def interpClosure(e:Exp): E[Closure] = for {
    v   <- interp(e)
    res <- v.fold(n => err(s"expected <function>, but got $n"), Closure(_,_).point[E])
  } yield res

  def mathInterp(l:Exp, r: Exp)(f: (Int, Int) => Int): E[RuntimeValue] =
    for {
      lx  <- interp(l)
      rx  <- interp(r)
      res <- math(lx, rx)(f)
    } yield res

  def math(l:RuntimeValue, r:RuntimeValue)
          (f: (Int, Int) => Int): E[RuntimeValue] = (l,r) match {
    case (NumV(lv), (NumV(rv))) => numV_E(f(lv,rv))
    case (badL,badR) => err(s"can't add: $badL + $badR")
  }

  def closure(f:Function, env: Env): RuntimeValue = Closure(f, env)
  def closure_E(f:Function, env: Env): E[RuntimeValue] = closure(f, env).point[E]

  def numV(i:Int): RuntimeValue = NumV(i)
  def numV_E(i:Int): E[RuntimeValue] = numV(i).point[E]

  def err[A](msg:String): E[A] = liftV(state(msg.left))
}
