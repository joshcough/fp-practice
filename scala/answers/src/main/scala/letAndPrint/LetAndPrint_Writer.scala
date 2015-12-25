package letAndPrint

import scala.language.higherKinds
import scalaz.Writer
import scalaz.std.list._
import scalaz.syntax.applicative._
import scalaz.syntax.foldable._
import scalaz.syntax.writer._
import LetAndPrint._

/**
  * Another example of the Writer Monad,
  * and an example of where we could use the Reader Monad too,
  * but we don't. That comes next.
  */
object LetAndPrint_Writer extends Interpreter {

  type W[A] = Writer[Output, A]

  def interpret(e:Exp): (Output, Int) = eval(e).run

  def lookup(v: String, env: Env): W[Int] =
    env.getOrElse(v, die(s"unbound variable: $v", env)).pure[W]

  def eval(exp: Exp, env: Env=Map()): W[Int]  =
    exp match {
      case Num (i)        => i.pure[W]
      case Add (l,r)      => for {
        lv <- eval(l,env)
        rv <- eval(r,env)
      } yield lv+rv
      case Mult(l,r)      => for {
        lv <- eval(l,env)
        rv <- eval(r,env)
      } yield lv*rv
      case Eq  (l,r) => for {
        lv <- eval(l, env)
        rv <- eval(r, env)
      } yield if (lv == rv) 1 else 0
      case If(predicate,tBranch,fBranch) => for {
        pv  <- eval(predicate, env)
        res <- eval(if (pv == 1) tBranch else fBranch, env)
      } yield res
      case Var (x)        => lookup(x, env)
      case Let ((x,e),b)  => for {
        ev <- eval(e, env)
        bv <- eval(b, env + (x -> ev))
      } yield bv
      case Print(e)       => for {
        ev <- eval(e, env)
        _  <- ev.set(List(ev.toString))
      } yield ev
      case Statements(es) =>
        es.foldlM(0)(_ => e => eval(e, env))
    }
}