package print

/**
  * FirstLang with Print statements added.
  * Print statements result in their values being collected in a list!
  * Not printed to std out. This is much more testable code.
  */
object PrintCollect {

  trait Exp
    case class Num  (i:Int)        extends Exp
    case class Add  (l:Exp, r:Exp) extends Exp
    case class Mult (l:Exp, r:Exp) extends Exp
    case class Print(e: Exp)       extends Exp

  type Output = List[String]

  def interp(node: Exp, output: Output=List()): (Output, Int)  =
    node match {
      case Num (i)   => ???
      case Add (l,r) => ???
      case Mult(l,r) => ???
      case Print(e)  => ???
    }
}