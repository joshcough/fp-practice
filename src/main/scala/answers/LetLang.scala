package answers

object LetLang {

  trait Exp
    case class Num(i:Int)                       extends Exp
    case class Add (l:Exp, r:Exp)               extends Exp
    case class Mult(l:Exp, r:Exp)               extends Exp
    case class Var (v: String)                  extends Exp
    case class Let (v: (String, Exp), body:Exp) extends Exp

  type Env = Map[String, Int]

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  def lookup(v: String, env: Env): Int =
    env.getOrElse(v, sys.error(s"unbound variable: $v, env: $env"))

  def interp(node: Exp, env: Env=Map()): Int = node match {
    case Num (i)   => i
    case Add (l,r) => interp(l,env) + interp(r,env)
    case Mult(l,r) => interp(l,env) * interp(r,env)
    case Var (x)   => lookup(x, env)
    case Let ((x,e),b) =>
      val eValue = interp(e, env)
      interp(b, env + (x -> eValue))
  }

  def run(node: Exp, expected: Int) = {
    val i = interp(node)
    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
  }
}
