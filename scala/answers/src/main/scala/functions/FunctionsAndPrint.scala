package functions

import Functions._

object FunctionsAndPrint {

  case class Print(e: Exp) extends Exp

  type Output = List[String]

  def lookup(v: String, env: Env, output: Output): RuntimeValue =
    env.getOrElse(v, sys.error(
      s"unbound variable, env: $env, output: ${output.mkString("\n")}"
    ))

  def eval(exp: Exp, env: Env=Map(),
             output: Output=List()): (Output, RuntimeValue) =
    exp match {
      case Num (i)       => (output, NumV(i))
      case Add (l,r)     =>
        val (lo,lv) = eval(l,env,output)
        val (ro,rv) = eval(r,env,lo)
        (ro,math(lv,rv)(_+_))
      case Mult(l,r)     =>
        val (lo,lv) = eval(l,env,output)
        val (ro,rv) = eval(r,env,lo)
        (ro,math(lv,rv)(_*_))
      case Eq  (l,r) =>
        val (lo,lv)  = eval(l,env,output)
        val (ro,rv)  = eval(r,env,lo)
        (ro, math(lv,rv)((ll,rr) => if (ll == rr) 1 else 0))
      case If(predicate,tBranch,fBranch) =>
        val (po,pv)  = eval(predicate,env,output)
        eval(if(pv == NumV(1)) tBranch else fBranch, env, po)
      case Var (x)       => (output, lookup(x, env, output))
      case Let ((x,e),b) =>
        val (nextOutput,eValue) = eval(e, env, output)
        eval(b, env + (x -> eValue), nextOutput)
      case Print(e)      =>
        val (nextOutput,eValue) = eval(e, env, output)
        val newString = eValue match {
          case NumV(i) => i.toString
          case Closure(_, _) => "<function>"
        }
        (nextOutput ++ List(newString), eValue)
      case Apply(fexp, a) => eval(fexp, env) match {
        case (o,Closure(func,cEnv)) =>
          val (o2,arg) = eval(a, env, o)
          eval(func.body, cEnv + (func.arg -> arg), o2)
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

}
