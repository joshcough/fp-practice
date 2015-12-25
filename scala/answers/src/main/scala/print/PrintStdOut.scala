package print

import PrintAST._

object PrintStdOut {

  def eval(exp: Exp): Int  = exp match {
    case Num (i)   => i
    case Add (l,r) => eval(l) + eval(r)
    case Mult(l,r) => eval(l) * eval(r)
    case Eq  (l,r) =>
      if(eval(l) ==  eval(r)) 1 else 0
    case If(predicate,tBranch,fBranch) =>
      val pv = eval(predicate)
      eval(if(pv == 1) tBranch else fBranch)
    case Print(e)  =>
      val i = eval(e)
      println(i)
      i
  }
}