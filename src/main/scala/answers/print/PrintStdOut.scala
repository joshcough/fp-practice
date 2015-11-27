package answers.print

/**
  * FirstLang with Print statements added.
  * Print statements result in their values being printed to std out.
  * TODO: add docs about why this is interesting
  */
object PrintStdOut {

  trait Exp
    case class Num  (i:Int)        extends Exp
    case class Add  (l:Exp, r:Exp) extends Exp
    case class Mult (l:Exp, r:Exp) extends Exp
    case class Print(e: Exp)       extends Exp

  type Output = List[String]

  def interp(node: Exp): Int  =
    node match {
      case Num (i)   => i
      case Add (l,r) => interp(l) + interp(r)
      case Mult(l,r) => interp(l) * interp(r)
      case Print(e)  =>
        val i = interp(e)
        println(i)
        i
    }
}