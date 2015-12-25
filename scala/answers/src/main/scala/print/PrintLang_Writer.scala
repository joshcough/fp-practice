package print

import PrintAST._

import scala.language.higherKinds
import scalaz.Writer
import scalaz.std.list._
import scalaz.syntax.applicative._
import scalaz.syntax.foldable._
import scalaz.syntax.writer._

/**
  * Example of the Writer Monad
  */
object PrintLang_Writer {

  case class Statements(es:List[Exp]) extends Exp

  type Env    = Map[String, Int]
  type Output = List[String]

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  type W[A] = Writer[Output, A]

  def die[A](msg: String, env: Env): A = sys.error(s"error: $msg, env: $env")

  def lookup(v: String, env: Env): W[Int] =
    env.getOrElse(v, die(s"unbound variable: $v", env)).pure[W]

  def eval(exp: Exp, env: Env=Map()): W[Int]  =
    exp match {
      case Num (i)        => i.pure[W]
      case Add (l,r)      => for {
        lv <- eval(l,env)
        rv <- eval(l,env)
      } yield lv+rv
      case Mult(l,r)      => for {
        lv <- eval(l,env)
        rv <- eval(l,env)
      } yield lv*rv
      case Print(e)       => for {
        ev <- eval(e, env)
        _  <- 0.set(List(ev.toString))
      } yield ev
      case Statements(es) => es.foldlM(0)(_ => e => eval(e, env))
    }
}
