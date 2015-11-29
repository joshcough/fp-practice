package functions

object FunctionsAndPrint {

  trait Exp {
    def apply(e:Exp) = Apply(this, e)
    def apply(i:Int) = Apply(this, Num(i))
  }
    case class Num(i:Int)                        extends Exp
    case class Add (l:Exp, r:Exp)                extends Exp
    case class Mult(l:Exp, r:Exp)                extends Exp
    case class Print(e: Exp)                     extends Exp
    case class Var (v: String)                   extends Exp
    case class Let (v: (String, Exp), body: Exp) extends Exp
    case class Function(arg: String,  body: Exp) extends Exp
    case class Apply(func: Exp, arg: Exp)        extends Exp

  sealed trait RuntimeValue
    case class NumV(i: Int) extends RuntimeValue
    case class Closure(f:Function, env: Env) extends RuntimeValue

  type Env    = Map[String, RuntimeValue]
  type Output = List[String]

  def lookup(v: String, env: Env, output: Output): RuntimeValue =
    env.getOrElse(v, sys.error(
      s"unbound variable, env: $env, output: ${output.mkString("\n")}"
    ))

  def interp(exp: Exp, env: Env=Map(),
             output: Output=List()): (Output, RuntimeValue) =
    exp match {
      case Num (i)       => (output, NumV(i))
      case Add (l,r)     =>
        val (lo,lv) = interp(l,env,output)
        val (ro,rv) = interp(r,env,lo)
        (ro,math(lv,rv)(_+_))
      case Mult(l,r)     =>
        val (lo,lv) = interp(l,env,output)
        val (ro,rv) = interp(r,env,lo)
        (ro,math(lv,rv)(_*_))
      case Var (x)       => (output, lookup(x, env, output))
      case Let ((x,e),b) =>
        val (nextOutput,eValue) = interp(e, env, output)
        interp(b, env + (x -> eValue), nextOutput)
      case Print(e)      =>
        val (nextOutput,eValue) = interp(e, env, output)
        val newString = eValue match {
          case NumV(i) => i.toString
          case Closure(_, _) => "<function>"
        }
        (nextOutput ++ List(newString), eValue)
      case Apply(fexp, a) => interp(fexp, env) match {
        case (o,Closure(func,cEnv)) =>
          val (o2,arg) = interp(a, env, o)
          interp(func.body, cEnv + (func.arg -> arg), o2)
        case (o,NumV(i)) => sys.error(s"$i is not a function. Output: $output")
      }
      case f:Function => (output, Closure(f, env))
    }

  def math(l:RuntimeValue, r:RuntimeValue)
          (f: (Int, Int) => Int): RuntimeValue =
    (l,r) match {
      case (NumV(lv), (NumV(rv))) => NumV(f(lv,rv))
      case bad => sys.error(s"can't add: $bad")
    }

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }
}
