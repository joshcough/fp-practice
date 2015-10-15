package writeme

object ArithLang {

  trait Exp
  case class Num(i:Int)         extends Exp
  case class Add (l:Exp, r:Exp) extends Exp
  case class Mult(l:Exp, r:Exp) extends Exp

  def interp(node: Exp): Int = node match {
    case Num (i)   => ???
    case Add (l,r) => ???
    case Mult(l,r) => ???
  }

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  def run(node: Exp, expected: Int) = {
    val i = interp(node)
    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
  }
}