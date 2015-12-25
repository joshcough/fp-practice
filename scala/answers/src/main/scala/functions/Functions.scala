package functions

object Functions {

  trait Exp {
    def apply(e:Exp) = Apply(this, e)
    def apply(i:Int) = Apply(this, Num(i))
  }
    case class Num(i:Int)                        extends Exp
    case class Add (l:Exp, r:Exp)                extends Exp
    case class Mult(l:Exp, r:Exp)                extends Exp
    case class Eq  (l:Exp, r:Exp)                extends Exp
    case class If  (p:Exp, t:Exp, f:Exp)         extends Exp
    case class Var (v: String)                   extends Exp
    case class Let (v: (String, Exp), body: Exp) extends Exp
    case class Function(arg: String,  body: Exp) extends Exp
    case class Apply(func: Exp, arg: Exp)        extends Exp

  sealed trait RuntimeValue
    case class NumV(i: Int) extends RuntimeValue
    case class Closure(f:Function, env: Env) extends RuntimeValue

  type Env = Map[String, RuntimeValue]

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  def lookup(v: String, env: Env): RuntimeValue =
    env.getOrElse(v, sys.error(s"unbound variable: $v, env: $env"))
}
