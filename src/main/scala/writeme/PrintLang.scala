package writeme

// Before Writer Monad
object PrintLang {

  trait Exp
  case class Num  (i:Int)                      extends Exp
  case class Add  (l:Exp, r:Exp)               extends Exp
  case class Mult (l:Exp, r:Exp)               extends Exp
  case class Print(e: Exp)                     extends Exp
  case class Statements(es:List[Exp])          extends Exp

  type Env    = Map[String, Int]
  type Output = List[String]

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  def interp(node: Exp,
             env: Env=Map(),
             output: Output=List()): (Output, Int)  =
    node match {
      case Num (i)        => ???
      case Add (l,r)      => ???
      case Mult(l,r)      => ???
      case Print(e)       => ???
      case Statements(es) => ???
    }

  def run(node: Exp, expected: Int) = {
    val (output,i) = interp(node)
    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
  }
}