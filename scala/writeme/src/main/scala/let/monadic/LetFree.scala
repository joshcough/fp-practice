package let.monadic

import scala.language.implicitConversions
import scalaz.{Free, State, ~>}

object LetFree {

  sealed trait Exp[A]
    final case class Num[A](v: Int, n: Int => A) extends Exp[A]
    final case class Add[A](l: Int, r: Int, n: Int => A) extends Exp[A]
    final case class Mult[A](l: Int, r: Int, n: Int => A) extends Exp[A]
    final case class Var[A](k: String, r: Int => A) extends Exp[A]
    final case class Let[A](k: String, v: Int, n: A) extends Exp[A]

  type LLProg[A] = Free.FreeC[Exp, A]
  implicit def liftExp[A](exp: Exp[A]): LLProg[A] = Free.liftFC(exp)

  def num(i: Int) = Num(i, identity)
  def add(i: Int, i2: Int) = Add(i, i2, identity)
  def mult(i: Int, i2: Int) = Mult(i, i2, identity)
  def gVar(k: String) = Var(k, identity)
  def let(k: String, v: Int) = Let(k, v, v)

  val test = for {
    x <- num(1)
    y <- add(2, x)
    _ <- let("foo", 3)
    z <- mult(4, y)
    a <- gVar("foo")
    b <- mult(a, z)
  } yield b

  type Env = Map[String, Int]
  type EnvState[A] = State[Env, A]

  def lookup(v: String, env: Env): Int =
    env.getOrElse(v, sys.error(s"unbound variable: $v, env: $env"))

  val arithInterp: Exp ~> EnvState = new (Exp ~> EnvState) {
    def apply[A](e: Exp[A]): EnvState[A]  = e match {
      case Num(v, n)      => ???
      case Add(v, v2, n)  => ???
      case Mult(v, v2, n) => ???
      case Var(s, n)      => ???
      case Let(k, v, n)   => ???
    }
  }

  def run = Free.runFC(test)(arithInterp).run(Map.empty[String, Int])._2
}
