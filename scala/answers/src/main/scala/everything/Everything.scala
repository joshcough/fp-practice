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

  def lookup(v: String, env: Env, output: Output, mem: Mem): RuntimeValue =
    env.getOrElse(v, sys.error(
      s"unbound variable, env: $env, mem: $mem, output: ${output.mkString("\n")}"
    ))

  def readMem(addr: Int, env: Env, output: Output, mem: Mem): RuntimeValue =
    mem.getOrElse(addr, sys.error(s"null pointer: $addr, env: $env"))

  type V = RuntimeValue

  def interp(exp: Exp, env: Env, out: Output, mem: Mem): (V,Output,Mem) =
    exp match {
      case Num (i)       => (NumV(i),out,mem)
      case Add (l,r)     =>
        val (lv,lo,mo1) = interp(l,env,out,mem)
        val (rv,ro,mo2) = interp(r,env,lo,mo1)
        (math(lv,rv)(_+_),ro,mo2)
      case Mult (l,r)    =>
        val (lv,lo,mo1) = interp(l,env,out,mem)
        val (rv,ro,mo2) = interp(r,env,lo,mo1)
        (math(lv,rv)(_*_),ro,mo2)
      case Var (x)       => (lookup(x, env, out, mem), out, mem)
      case Let ((x,e),b) =>
        val (eValue,o1,m1) = interp(e, env, out, mem)
        interp(b, env + (x -> eValue), o1, m1)
      case Print(e)      =>
        val (eValue,o1,m1) = interp(e, env, out, mem)
        val newString = eValue match {
          case NumV(i) => i.toString
          case Closure(_, _) => "<function>"
        }
        (eValue, o1 ++ List(newString), m1)
      case Apply(fexp, a) => interp(fexp, env, out, mem) match {
        case (Closure(func,cEnv),o1,m1) =>
          val (arg,o2,m2) = interp(a, env, o1, m1)
          interp(func.body, cEnv + (func.arg -> arg), o2, m2)
        // TODO: write generic error printing thing
        case (NumV(i),o1,m1) => sys.error(s"$i is not a function. Output: $o1, mem: $mem")
      }
      case f:Function => (Closure(f, env), out, mem)
      case SetMem(address: Exp, e:Exp) =>
        val (addrValue,o1,m1) = interp(address, env, out, mem)
        val (eValue,o2,m2) = interp(e, env, o1, m1)
        // we can return any value here...
        // i chose to have set return 0 always.
        addrValue match {
          case NumV(addr) =>  (NumV(0), out, m2 + (addr -> eValue))
          case c => sys.error(s"invalid memory address: $c. Output: $o1, mem: $mem")
        }
      case GetMem(address: Exp) =>
        val (addrValue,o1,m1) = interp(address, env, out, mem)
        addrValue match {
          case NumV(addr) =>  (readMem(addr, env, o1, m1), o1, m1)
          case c => sys.error(s"invalid memory address: $c. Output: $o1, mem: $mem")
        }
      case Statements(es)    =>
        es.foldLeft((NumV(0):RuntimeValue,out,mem)){
          case ((_,outacc, memacc),e) => interp(e, env, outacc, memacc)
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
