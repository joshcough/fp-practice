package startHere

/**
  * Start Here
  */
object FirstLang {

  trait Exp
    case class Num(i:Int)         extends Exp
    case class Add (l:Exp, r:Exp) extends Exp
    case class Mult(l:Exp, r:Exp) extends Exp

  /**
    * FILL ME IN
    * @param node
    * @return
    */
  def interp(node: Exp): Int = node match {
    case Num (i)   => ???
    case Add (l,r) => ???
    case Mult(l,r) => ???
  }
}