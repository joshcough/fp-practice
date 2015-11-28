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

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  def lookup(v: String, env: Env): RuntimeValue =
    env.getOrElse(v, sys.error(s"unbound variable: $v, env: $env"))

  def interp(node: Exp, env: Env=Map()): RuntimeValue = node match {
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
                  (f: (Int, Int) => Int) = (l,r) match {
    case (NumV(lv), (NumV(rv))) => NumV(f(lv,rv))
    case bad => sys.error(s"can't add: $bad")
  }

  def run(node: Exp, expected: RuntimeValue) = {
    val i = interp(node)
    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
    else println(i)
  }

  def main (args: Array[String]): Unit = {
    run(
      Apply(
        Function("x", Add(n"7", v"x")),
        Num(5)
      ),
      NumV(12)
    )
    // ((def (x) ((def (x) (+ x x)) x)) 5)
    run(
      Apply(
        Function("x", Add(v"x", Apply(
          Function("x", Add(v"x", v"x")),
          v"x"
        ))),
        Num(5)
      ),
      NumV(15)
    )
    // (\x -> \y -> \x -> x) 6 7 8
    run(
      Apply(Apply(Apply(
        Function("x", Function("y", Function("x", v"x")))
      , n"6"), n"7"), n"8"),
      NumV(8)
    )
  }
}
