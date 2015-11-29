package functions

object Functions {

  trait Exp {
    def apply(e:Exp) = Apply(this, e)
    def apply(i:Int) = Apply(this, Num(i))
  }
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

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  def lookup(v: String, env: Env): RuntimeValue =
    env.getOrElse(v, sys.error(s"unbound variable: $v, env: $env"))

  def interp(exp: Exp, env: Env=Map()): RuntimeValue = exp match {
    case Num (i)   => NumV(i)
    case Add (l,r) => math(interp(l,env), interp(r,env))(_+_)
    case Mult(l,r) => math(interp(l,env), interp(r,env))(_*_)
    case Var (x)   => lookup(x, env)
    case Let ((x,e),b)  => interp(b, env + (x -> interp(e, env)))
    case Apply(fexp, a) => interp(fexp, env) match {
      case Closure(func,cEnv) =>
        interp(func.body, cEnv + (func.arg -> interp(a, env)))
      case NumV(i) => sys.error(s"$i is not a function.")
    }
    case f:Function => Closure(f, env)
  }

  def math(l:RuntimeValue, r:RuntimeValue)
          (f: (Int, Int) => Int): RuntimeValue = (l,r) match {
    case (NumV(lv), (NumV(rv))) => NumV(f(lv,rv))
    case bad => sys.error(s"can't add: $bad")
  }
}
