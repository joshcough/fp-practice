package everything

/**
  * Created by jcough on 11/29/15.
  * Functions, Let bindings, Memory, Output, Statement blocks.
  */
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

  sealed trait RuntimeValue
    case class NumV(i: Int) extends RuntimeValue
    case class Closure(f:Function, env: Env) extends RuntimeValue

  type Env    = Map[String, RuntimeValue]
  type Output = List[String]
  type Mem    = Map[Int,RuntimeValue]

  def lookup(v: String, state:State): RuntimeValue =
    state.env.getOrElse(v, sys.error(s"unbound variable, $v in $state"))

  def readMem(addr: Int, env: Env, output: Output, mem: Mem): RuntimeValue =
    mem.getOrElse(addr, sys.error(s"null pointer: $addr, env: $env"))

  type V = RuntimeValue
  case class State(env: Env, out: Output, mem: Mem)

  def interp(exp: Exp, state: State): (V,Output,Mem) =
    exp match {
      case Num (i)       => (NumV(i),state.out,state.mem)
      case Add (l,r)     =>
        val (lv,lo,mo1) = interp(l,state)
        val (rv,ro,mo2) = interp(r,State(state.env,lo,mo1))
        (math(lv,rv)(_+_),ro,mo2)
      case Mult (l,r)    =>
        val (lv,lo,mo1) = interp(l,state)
        val (rv,ro,mo2) = interp(r,State(state.env,lo,mo1))
        (math(lv,rv)(_*_),ro,mo2)
      case Var (x)       => (lookup(x, state), state.out, state.mem)
      case Let ((x,e),b) =>
        val (eValue,o1,m1) = interp(e, state)
        interp(b, State(state.env + (x -> eValue), o1, m1))
      case Print(e)      =>
        val (eValue,o1,m1) = interp(e, state)
        val newString = eValue match {
          case NumV(i) => i.toString
          case Closure(_, _) => "<function>"
        }
        (eValue, o1 ++ List(newString), m1)
      case Apply(fexp, a) => interp(fexp, state) match {
        case (Closure(func,cEnv),o1,m1) =>
          val (arg,o2,m2) = interp(a, State(state.env, o1, m1))
          interp(func.body, State(cEnv + (func.arg -> arg), o2, m2))
        // TODO: write generic error printing thing
        case (NumV(i),o1,m1) => sys.error(s"$i is not a function. State: $state")
      }
      case f:Function => (Closure(f, state.env), state.out, state.mem)
      case SetMem(address: Exp, e:Exp) =>
        val (addrValue,o1,m1) = interp(address, state)
        val (eValue,o2,m2) = interp(e, State(state.env, o1, m1))
        // we can return any value here...
        // i chose to have set return 0 always.
        addrValue match {
          case NumV(addr) =>  (NumV(0), state.out, m2 + (addr -> eValue))
          case c => sys.error(s"invalid memory address: $c. State: $state")
        }
      case GetMem(address: Exp) =>
        val (addrValue,o1,m1) = interp(address, state)
        addrValue match {
          case NumV(addr) =>  (readMem(addr, state.env, o1, m1), o1, m1)
          case c => sys.error(s"invalid memory address: $c. State: $state")
        }
      case Statements(es)    =>
        es.foldLeft((NumV(0):RuntimeValue,state.out,state.mem)){
          case ((_,outacc, memacc),e) => interp(e, State(state.env, outacc, memacc))
        }
    }

  def math(l:RuntimeValue, r:RuntimeValue)
          (f: (Int, Int) => Int): RuntimeValue = (l,r) match {
    case (NumV(lv), (NumV(rv))) => NumV(f(lv,rv))
    case bad => sys.error(s"can't add: $bad")
  }

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }
}
