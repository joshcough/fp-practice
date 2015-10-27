package answers

import scalaz._
import scalaz.std.anyVal.intInstance
import scala.language.implicitConversions

object HOFFreeLang {

  trait Exp[A]
    case class Prim[A](a: A)                              extends Exp[A]
    case class Add [A](l:Exp[A], r:Exp[A])                extends Exp[A]
    case class Var [A](v: String)                         extends Exp[A]
    case class Let [A](v: (String, Exp[A]), body: Exp[A]) extends Exp[A]
    case class Function[A](arg: String,  body: Exp[A])    extends Exp[A]
    case class Apply[A](func: Exp[A], arg: Exp[A])        extends Exp[A]

  implicit val ExpFunctor = new Functor[Exp] {
    def map[A, B](exp: Exp[A])(f: A => B): Exp[B] = exp match {
      case Prim(a)        => Prim(f(a))
      case Add (l,r)      => Add (map(l)(f), map(r)(f))
      case Var (x)        => Var(x)
      case Let ((x,e),b)  => Let((x, map(e)(f)), map(b)(f))
      case Apply(fun, a)  => Apply(map(fun)(f),  map(a)(f))
      case Function(a, b) => Function(a, map(b)(f))
    }
  }

  trait RuntimeValue[A]
    case class PrimV  [A](a: A)                       extends RuntimeValue[A]
    case class Closure[A](f:Function[A], env: Env[A]) extends RuntimeValue[A]

  type Env[A] = Map[String, RuntimeValue[A]]

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v[A](args: Any*): Var[A]    = Var(sc.parts.mkString)
    def n   (args: Any*): Prim[Int] = Prim(sc.parts.mkString.toInt)
  }

  def lookup[A](v: String, env: Env[A]): RuntimeValue[A] =
    env.getOrElse(v, sys.error(s"unbound variable: $v, env: $env"))

  def interp[A](node: Exp[A], env:  Env[A])
               (implicit m: Monoid[A]): RuntimeValue[A] = node match {
    case Prim(a)        => PrimV(a)
    case Add (l,r)      => math(interp(l,env), interp(r,env))
    case Var (x)        => lookup(x, env)
    case Let ((x,e),b)  => interp(b, env + (x -> interp(e, env)))
    case Apply(fexp, a) => interp(fexp, env) match {
      case Closure(func,cEnv) =>
        interp(func.body, cEnv + (func.arg -> interp(a, env)))
      case PrimV(bad) => sys.error(s"$bad is not a function.")
    }
    case f@(Function(_, _)) => Closure(f, env)
  }

  def math[A](l:RuntimeValue[A], r:RuntimeValue[A])(implicit m: Monoid[A]) =
    (l,r) match {
      case (PrimV(lv), (PrimV(rv))) => PrimV(m.append(lv,rv))
      case bad => sys.error(s"can't combine: $bad")
    }

  def run[A](node: Exp[A], expected: RuntimeValue[A])(implicit m: Monoid[A]) = {
    val i: RuntimeValue[A] = interp[A](node, Map())
    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
    else println(i)
  }

  def main (args: Array[String]): Unit = {
    type LLProg[A] = Free.FreeC[Exp, A]
    implicit def liftExp[A](exp: Exp[A]): LLProg[A] = Free.liftFC(exp)
    val test = for {
      x <- Prim(1)
      y <- add(2, x)
    } yield y
//    val x = Free.runFC(test)(interp).run(Map.empty[String, Int])._2
//    println(x)
  }

  def add[A](l: A, r: A)(implicit m: Monoid[A]) =
    Add(Prim(l), Prim(r))
}
