package everything

import Everything._

import scalaz.\/
import scalaz.syntax.either._

/**
  * Created by jcough on 11/29/15.
  * Functions, Let bindings, Memory, Output, Statement blocks.
  */
object EverythingNaiveInterpreter extends Interpreter {

  case class ProgramException(msg: String, state: ProgramState)
    extends RuntimeException(msg)

  override def interpret(exp: Everything.Exp): (String \/ RuntimeValue, ProgramState) =
      try {
        val t = interp(exp, ProgramState())
        (t._1.right, ProgramState(env = Map(), t._2, t._3))
      }
      catch { case p: ProgramException => (p.msg.left, p.state) }

  def lookup(v: String, state:ProgramState): RuntimeValue =
    state.env.getOrElse(v, err(s"unbound variable, $v", state))

  def readMem(addr: Int, state:ProgramState): RuntimeValue =
    state.mem.getOrElse(addr, err(s"null pointer: $addr", state))

  def interp(exp: Exp, state: ProgramState): (RuntimeValue,Output,Mem) =
    exp match {
      case Num (i)       => (NumV(i),state.out,state.mem)
      case Add (l,r)     =>
        val (lv,lo,mo1) = interp(l,state)
        val (rv,ro,mo2) = interp(r,ProgramState(state.env,lo,mo1))
        (math(lv,rv,state)(_+_),ro,mo2)
      case Mult (l,r)    =>
        val (lv,lo,mo1) = interp(l,state)
        val (rv,ro,mo2) = interp(r,ProgramState(state.env,lo,mo1))
        (math(lv,rv,state)(_*_),ro,mo2)
      case Var (x)       => (lookup(x, state), state.out, state.mem)
      case Let ((x,e),b) =>
        val (eValue,o1,m1) = interp(e, state)
        interp(b, ProgramState(state.env + (x -> eValue), o1, m1))
      case Print(e)      =>
        val (eValue,o1,m1) = interp(e, state)
        val newString = eValue match {
          case NumV(i) => i.toString
          case Closure(_, _) => "<function>"
        }
        (eValue, o1 ++ List(newString), m1)
      case Apply(fexp, a) => interp(fexp, state) match {
        case (Closure(func,cEnv),o1,m1) =>
          val (arg,o2,m2) = interp(a, ProgramState(state.env, o1, m1))
          interp(func.body, ProgramState(cEnv + (func.arg -> arg), o2, m2))
        // TODO: write generic error printing thing
        case (NumV(i),o1,m1) => err(s"$i is not a function", state)
      }
      case f:Function => (Closure(f, state.env), state.out, state.mem)
      case SetMem(address: Exp, e:Exp) =>
        val (addrValue,o1,m1) = interp(address, state)
        val (eValue,o2,m2) = interp(e, ProgramState(state.env, o1, m1))
        // we can return any value here...
        // i chose to have set return 0 always.
        addrValue match {
          case NumV(addr) =>  (NumV(0), state.out, m2 + (addr -> eValue))
          case c => err(s"invalid memory address: $c", state)
        }
      case GetMem(address: Exp) =>
        val (addrValue,o1,m1) = interp(address, state)
        addrValue match {
          case NumV(addr) => (readMem(addr, ProgramState(state.env, o1, m1)), o1, m1)
          case c => err(s"invalid memory address: $c", state)
        }
      case Statements(es)    =>
        es.foldLeft((NumV(0):RuntimeValue,state.out,state.mem)){
          case ((_,outacc, memacc),e) =>
            interp(e, ProgramState(state.env, outacc, memacc))
        }
    }

  def math(l:RuntimeValue, r:RuntimeValue, s:ProgramState)
          (f: (Int, Int) => Int): RuntimeValue = (l,r) match {
    case (NumV(lv), (NumV(rv))) => NumV(f(lv,rv))
    case bad => err(s"can't add: $bad", s)
  }

  def err(msg: String, s: ProgramState) = throw new ProgramException(s"$msg", s)
}

