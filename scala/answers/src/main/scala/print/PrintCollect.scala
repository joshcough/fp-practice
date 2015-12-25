package print

import PrintAST._

/**
  * FirstLang with Print statements added.
  * Print statements result in their values being collected in a list!
  * Not printed to std out. This is much more testable code.
  */
object PrintCollect {

  def eval(exp: Exp, output: Output=List()): (Output, Int) =
    exp match {
      case Num (i)   => (output, i)
      case Add (l,r) =>
        val (lo,lv)  = eval(l,output)
        val (ro,rv)  = eval(r,lo)
        (ro,lv+rv)
      case Mult(l,r) =>
        val (lo,lv)  = eval(l,output)
        val (ro,rv)  = eval(r,lo)
        (ro,lv*rv)
      case Eq  (l,r) =>
        val (lo,lv)  = eval(l,output)
        val (ro,rv)  = eval(r,lo)
        (ro, if(lv == rv) 1 else 0)
      case If(predicate,tBranch,fBranch) =>
        val (po,pv)  = eval(predicate,output)
        eval(if(pv == 1) tBranch else fBranch, po)
      case Print(e)  =>
        val (nextOutput,eValue) = eval(e,  output)
        (nextOutput ++ List(eValue.toString),eValue)
    }
}