//package everything
//
//import everything.Everything.Apply
//import everything.Everything._
//
//import scala.language.higherKinds
//import scalaz.State._
//import scalaz.{State, EitherT, StateT, WriterT, Reader, \/}
//import scalaz.std.list._
//import scalaz.syntax.either._
//import scalaz.syntax.monad._
//import scalaz.syntax.traverse._
//
///**
// * I have - ReaderT/Writer, and EitherT/State
// * I need, at the very end, (ProgramState, String \/ RuntimeValue)
// * which is provided by EitherT/State.
// * Oh, but I also need the Output, so I need:
// * - ((Output, ProgramState), String \/ RuntimeValue)
// * I think if I go with EitherT/StateT/Writer, I'll get:
// * - (Output, (ProgramState, String \/ RuntimeValue))
// * Okay, that's cool, it makes sense. Now, can we use ReaderT?
// * EitherT/StateT/WriterT


//[20:54:18]  <edwardk>	EitherT e (State s)
//[20:55:17]  <edwardk>	running that with an input state gives you back a pair of Either e a  and a state
//[20:55:43]  <joshcough>	EitherT  gives you back a pair?
//[20:55:44]  <edwardk>	on the other hand StateT s (Either e) will give back back Either e (a, s)
//[20:56:18]  <edwardk>	so EitherT e (State s) a ~ s -> (Either e a, s)    while StateT s (Either e) a ~ s -> Either e (a, s)
//  */
//object EverythingZInterpreter3 extends Interpreter {
//
//  override def interpret(exp: Exp): (String \/ RuntimeValue, ProgramState) = {
//    val x = eval(exp)
//    val y: (Output, (Mem, String \/ RuntimeValue)) = x.run.run(Map()).run.run(Map())
//    (y._2._2, ProgramState(Map(), y._1, y._2._1))
//  }
//
//  type Memory = Mem
//
//  type R[A] = Reader [Env, A]
//  type W[A] = WriterT[R, Output, A]
//  type S[A] = StateT [W, Memory, A]
//  type E[A] = EitherT[S, String, A]
//
//  def eval(exp: Exp): E[RuntimeValue] =
//    exp match {
//      case Num (i)        => numV_E(i)
//      case Add (l,r)      => mathInterp(l, r)(_+_)
//      case Mult(l,r)      => mathInterp(l, r)(_*_)
//      case Var (x)        => lookup(x)
//      case Let ((x,e),b)  => eval(Apply(Function(x, b), e))
//      case Print(e)       => for { ev <- eval(e); _ <- print(ev) } yield ev
//      case Apply(fexp, a) => for {
//        c   <- interpClosure(fexp)
//        v   <- eval(a)
//        //res <- localE(_ => c.env + (c.f.arg -> v))(eval(c.f.body))
//      } yield ???
//      case f:Function     => ??? //TODO lift(get[Mem].map(p => closure(f, p.env)))
//      case SetMem(address, e) => for {
//        addr   <- eval(address)
//        newVal <- eval(e)
//        res    <- withAddress(addr)(writeMem(_, newVal))
//      } yield numV(0)
//      case GetMem(address) => for {
//        addr <- eval(address)
//        res  <- withAddress(addr)(readMem)
//      } yield res
//      case Statements(es) =>
//        // for empty statement blocks, we just return 0.
//        es.traverse(eval).map(_.lastOption.getOrElse(numV(0)))
//    }
//
//  def liftV[A](s:S[String \/ A]): E[A] = EitherT[S, String, A](s)
////  def liftM[G[_[_], _]](implicit G: MonadTrans[G]): G[F, A] = G.liftM(self)
////  def liftS[A](s:State[Mem, A]) : E[A] = liftV(s.map(_.right).liftM[R])
//
//  def lift[A](s:S[A]): E[A] = liftV(s.map(_.right))
//
//
//
//  def usingState[A](lens: Mem => Option[RuntimeValue],
//                    failure: Mem => String): E[RuntimeValue] = ???
////    liftV(get[Mem].map(s =>
////      lens(s) match { case Some(r) => r.right; case _ => failure(s).left }
////    ))
//
////  def localE[A](f: Env => Env)(action: E[RuntimeValue]): E[RuntimeValue] =
////    liftV(localS(f)(action.run))
////
////  def localS[A](f: Env => Env)(action: => S[A]): S[A] =
////    for {
////      old <- get[Mem]
////      _   <- put[Mem](old.copy(env = f(old.env)))
////      res <- action
////      _   <- modify[Mem](_.copy(env = old.env))
////    } yield res
//
//  def lookup(v: String): E[RuntimeValue] = ???
////    usingState(_.env.get(v), s => s"unbound variable: $v in $s")
//
//  def readMem(addr: Int): E[RuntimeValue] =
//    usingState(_.get(addr), s => s"null pointer: $addr in $s")
//
//  def mod(f: Mem => Mem): E[Unit] = liftS(modify[Mem](f))
//
//  def writeMem(addr: Int, rv: RuntimeValue): E[Unit] = mod(_ + (addr -> rv))
//
//  def print(rv: RuntimeValue): E[Unit] =
//    ???(mod(ps => ps.copy(out = ps.out ++ List(rv.toString))))
//
//
//  def withAddress[A](addr: RuntimeValue)(f: Int => E[A]): E[A] =
//    addr.fold(f(_), (_,_) => err("<function> is not an address"))
//
//  def interpClosure(e:Exp): E[Closure] = for {
//    v   <- eval(e)
//    res <- v.fold(n => err(s"expected <function>, but got $n"), Closure(_,_).point[E])
//  } yield res
//
//  def mathInterp(l:Exp, r: Exp)(f: (Int, Int) => Int): E[RuntimeValue] =
//    for {
//      lx  <- eval(l)
//      rx  <- eval(r)
//      res <- math(lx, rx)(f)
//    } yield res
//
//  def math(l:RuntimeValue, r:RuntimeValue)
//          (f: (Int, Int) => Int): E[RuntimeValue] = (l,r) match {
//    case (NumV(lv), (NumV(rv))) => numV_E(f(lv,rv))
//    case (badL,badR) => err(s"can't add: $badL + $badR")
//  }
//
//  def closure(f:Function, env: Env): RuntimeValue = Closure(f, env)
//  def closure_E(f:Function, env: Env): E[RuntimeValue] = closure(f, env).point[E]
//
//  def numV(i:Int): RuntimeValue = NumV(i)
//  def numV_E(i:Int): E[RuntimeValue] = numV(i).point[E]
//
//  def err[A](msg:String): E[A] = liftV(state(msg.left))
//}
