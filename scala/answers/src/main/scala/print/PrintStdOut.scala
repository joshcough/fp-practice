package print

object PrintStdOut {

  trait Exp
    case class Num  (i:Int)        extends Exp
    case class Add  (l:Exp, r:Exp) extends Exp
    case class Mult (l:Exp, r:Exp) extends Exp
    case class Print(e: Exp)       extends Exp

  def interp(exp: Exp): Int  = exp match {
    case Num (i)   => i
    case Add (l,r) => interp(l) + interp(r)
    case Mult(l,r) => interp(l) * interp(r)
    case Print(e)  =>
      val i = interp(e)
      println(i)
      i
  }
}