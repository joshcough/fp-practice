package functions

import Functions._

/**
  * Created by jcough on 12/24/15.
  */
object FunctionsNaive {

  def eval(exp: Exp, env: Env=Map()): RuntimeValue = exp match {
    case Num (i)   => NumV(i)
    case Add (l,r) => mathInterp(l,r,env)(_+_)
    case Mult(l,r) => mathInterp(l,r,env)(_*_)
    case Eq(l,r)   => mathInterp(l,r,env)((ll,rr) => if(ll==rr) 1 else 0)
    case If(predicate,tBranch,fBranch) =>
      eval(predicate) match {
        case NumV(i) => eval(if(i==1) tBranch else fBranch)
        case bad => sys.error(s"predicate not a number: $bad")
      }
    case Var (x)   => lookup(x, env)
    case Let ((x,e),b)  => eval(b, env + (x -> eval(e, env)))
    case Apply(fexp, a) => eval(fexp, env) match {
      case Closure(func,cEnv) =>
        eval(func.body, cEnv + (func.arg -> eval(a, env)))
      case NumV(i) => sys.error(s"$i is not a function.")
    }
    case f:Function => Closure(f, env)
  }

  def mathInterp(l:Exp, r:Exp, env: Env)
                (f: (Int, Int) => Int): RuntimeValue =
    math(eval(l,env), eval(r,env))(f)

  def math(l:RuntimeValue, r:RuntimeValue)
          (f: (Int, Int) => Int): RuntimeValue = (l,r) match {
    case (NumV(lv), (NumV(rv))) => NumV(f(lv,rv))
    case bad => sys.error(s"can't do math with: $bad")
  }
}