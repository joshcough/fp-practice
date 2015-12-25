package let

/**
  *
  */
object LetLang {

  trait Exp
    case class Num (i:Int)                      extends Exp
    case class Add (l:Exp, r:Exp)               extends Exp
    case class Mult(l:Exp, r:Exp)               extends Exp
    case class Eq  (l:Exp, r: Exp)              extends Exp
    case class If  (p:Exp, t:Exp, f:Exp)        extends Exp
    case class Var (v: String)                  extends Exp
    case class Let (v: (String, Exp), body:Exp) extends Exp

  type Env = Map[String, Int]

  def lookup(v: String, env: Env): Int =
    env.getOrElse(v, sys.error(s"unbound variable: $v, env: $env"))

  def eval(exp: Exp, env: Env=Map()): Int = exp match {
    case Num (i)   => i
    case Add (l,r) => eval(l,env) + eval(r,env)
    case Mult(l,r) => eval(l,env) * eval(r,env)
    case Eq  (l,r) =>
      if(eval(l,env) == eval(r, env)) 1 else 0
    case If(predicate,tBranch,fBranch) =>
      val pv = eval(predicate)
      eval(if(pv == 1) tBranch else fBranch)
    case Var (x)   => lookup(x, env)
    case Let ((x,e),b) =>
      val eValue = eval(e, env)
      eval(b, env + (x -> eValue))
  }
}
