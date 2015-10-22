package writeme

object LetAndPrintLang {

  trait Exp
    case class Num  (i:Int)                      extends Exp
    case class Add  (l:Exp, r:Exp)               extends Exp
    case class Mult (l:Exp, r:Exp)               extends Exp
    case class Var  (v: String)                  extends Exp
    case class Let  (v: (String, Exp), body:Exp) extends Exp
    case class Print(e: Exp)                     extends Exp
    case class Statements(es:List[Exp])          extends Exp

  type Env    = Map[String, Int]
  type Output = List[String]

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  def die[A](msg: String, env: Env, out: Output): A =
    sys.error(s"error: $msg, env: $env, output: ${out.mkString("\n")}")

  def lookup(v: String, env: Env, output: Output): Int =
    env.getOrElse(v, die(s"unbound variable: $v", env, output))

  def interp(node: Exp,
             env: Env=Map(),
             output: Output=List()): (Output, Int)  =
    node match {
      case Num (i)        => ???
      case Add (l,r)      => ???
      case Mult(l,r)      => ???
      case Var (x)        => ???
      case Let ((x,e),b)  => ???
      case Print(e)       => ???
      case Statements(es) => ???
    }

  def run(node: Exp, expected: Int) = {
    val (output,i) = interp(node)
    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
  }
}
