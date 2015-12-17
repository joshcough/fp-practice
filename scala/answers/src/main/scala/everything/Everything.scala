package everything

import scalaz.{\/, Show}

object Everything {

  sealed trait Exp {
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
    case class SetMem(address: Exp, e: Exp)      extends Exp
    case class GetMem(address: Exp)              extends Exp
    case class Statements(es:List[Exp])          extends Exp

  sealed trait RuntimeValue {
    def fold[A](f: Int => A, g: (Function, Env) => A): A = this match {
      case NumV(i) => f(i)
      case Closure(fun, e) => g(fun, e)
    }
    override def toString = ShowRV.show(this).toString
  }
    case class NumV(i: Int) extends RuntimeValue
    case class Closure(f:Function, env: Env) extends RuntimeValue

  type Env    = Map[String, RuntimeValue]
  type Output = List[String]
  type Mem    = Map[Int,RuntimeValue]

  // stands for ProgramState, but thats too long.
  case class PState(env: Env = Map(), out: Output = List(), mem: Mem = Map())

  trait Interpreter {
    def interpret(exp: Exp): (String \/ RuntimeValue, PState)
  }

  implicit lazy val ShowRV: Show[RuntimeValue] = Show.show {
    case NumV(i) => i.toString
    case Closure(_, _) => "<function>"
  }

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }
}

