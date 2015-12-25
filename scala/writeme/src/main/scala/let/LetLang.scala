package let

/**
  *
  */
object LetLang {

  trait Exp
    case class Num(i:Int)                       extends Exp
    case class Add (l:Exp, r:Exp)               extends Exp
    case class Mult(l:Exp, r:Exp)               extends Exp
    case class Var (v: String)                  extends Exp
    case class Let (v: (String, Exp), body:Exp) extends Exp

  type Env = Map[String, Int]

  def lookup(v: String, env: Env): Int =
    env.getOrElse(v, sys.error(s"unbound variable: $v, env: $env"))

  def eval(exp: Exp, env: Env=Map()): Int = exp match {
    case Num (i)       => ???
    case Add (l,r)     => ???
    case Mult(l,r)     => ???
    case Var (x)       => ???
    case Let ((x,e),b) => ???
  }
}
