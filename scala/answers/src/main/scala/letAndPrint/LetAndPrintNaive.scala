package letAndPrint

import LetAndPrint._

/**
  * Created by jcough on 12/24/15.
  */
object LetAndPrintNaive extends Interpreter {

  def interpret(e:Exp): (Output, Int) = eval(e)

  def lookup(v: String, env: Env, output: Output): Int =
    env.getOrElse(v, die(s"unbound variable: $v", env))

  def eval(exp: Exp, env: Env=Map(),
             output: Output=List()): (Output, Int) =
    exp match {
      case Num (i)       => (output, i)
      case Add (l,r)     =>
        val (lo,lv) = eval(l,env,output)
        val (ro,rv) = eval(r,env,lo)
        (ro,lv+rv)
      case Mult(l,r)     =>
        val (lo,lv) = eval(l,env,output)
        val (ro,rv) = eval(r,env,lo)
        (ro,lv*rv)
      case Eq  (l,r) =>
        val (lo,lv)  = eval(l,env,output)
        val (ro,rv)  = eval(r,env,lo)
        (ro, if(lv == rv) 1 else 0)
      case If(predicate,tBranch,fBranch) =>
        val (po,pv)  = eval(predicate,env,output)
        eval(if(pv == 1) tBranch else fBranch, env, po)
      case Var (x)       => (output, lookup(x, env, output))
      case Let ((x,e),b) =>
        val (nextOutput,eValue) = eval(e, env, output)
        eval(b, env + (x -> eValue), nextOutput)
      case Print(e)      =>
        val (nextOutput,eValue) = eval(e, env, output)
        (nextOutput ++ List(eValue.toString), eValue)
      case Statements(es) =>
        es.foldLeft((output,0)){ case ((outacc,_),e) =>
          eval(e, env, outacc)
        }
    }
}
