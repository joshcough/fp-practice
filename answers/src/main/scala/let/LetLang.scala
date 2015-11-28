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

  def interp(node: Exp, env: Env=Map()): Int = node match {
    case Num (i)   => i
    case Add (l,r) => interp(l,env) + interp(r,env)
    case Mult(l,r) => interp(l,env) * interp(r,env)
    case Var (x)   => lookup(x, env)
    case Let ((x,e),b) =>
      val eValue = interp(e, env)
      interp(b, env + (x -> eValue))
  }
}
