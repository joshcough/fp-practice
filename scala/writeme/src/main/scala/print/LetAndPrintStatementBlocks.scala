package print

object LetAndPrintStatementBlocks {

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

  def lookup(v: String, env: Env, output: Output): Int =
    env.getOrElse(v, sys.error(
      s"unbound variable: $v. env: $env, output: ${output.mkString("\n")}")
    )

  def interp(exp: Exp, env: Env=Map(),
             output: Output=List()): (Output, Int)  =
    exp match {
      case Num (i)        => ???
      case Add (l,r)      => ???
      case Mult(l,r)      => ???
      case Var (x)        => ???
      case Let ((x,e),b)  => ???
      case Print(e)       => ???
      case Statements(es) => ???
    }
}
