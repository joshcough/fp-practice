package functions

object Functions {

  trait Exp
    case class Num(i:Int)                        extends Exp
    case class Add (l:Exp, r:Exp)                extends Exp
    case class Mult(l:Exp, r:Exp)                extends Exp
    case class Var (v: String)                   extends Exp
    case class Let (v: (String, Exp), body: Exp) extends Exp
    case class Function(arg: String,  body: Exp) extends Exp
    case class Apply(func: Exp, arg: Exp)        extends Exp

  trait RuntimeValue
    case class NumV(i: Int) extends RuntimeValue
    case class Closure(f:Function, env: Env) extends RuntimeValue

  type Env = Map[String, RuntimeValue]

  def lookup(v: String, env: Env): RuntimeValue =
    env.getOrElse(v, sys.error(s"unbound variable: $v, env: $env"))

  def interp(exp: Exp, env: Env=Map()): RuntimeValue = exp match {
    case Num (i)        => ???
    case Add (l,r)      => ???
    case Mult(l,r)      => ???
    case Var (x)        => ???
    case Let ((x,e),b)  => ???
    case Apply(fexp, a) => ???
    case f:Function     => ???
  }

  def math(l:RuntimeValue, r:RuntimeValue)
                  (f: (Int, Int) => Int) = (l,r) match {
    case (NumV(lv), (NumV(rv))) => NumV(f(lv,rv))
    case bad => sys.error(s"can't do math: $bad")
  }

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }
}
