package let.monadic

/**
  * We'll implement several interpreters for this new AST.
  * I'm providing an object holding the AST and helper functions
  * for it, so that they don't have to be repeated in each interpreter.
  */
object LetAST {

  trait Exp
    case class Num(i:Int)                       extends Exp
    case class Add (l:Exp, r:Exp)               extends Exp
    case class Mult(l:Exp, r:Exp)               extends Exp
    case class Var (v: String)                  extends Exp
    case class Let (v: (String, Exp), body:Exp) extends Exp

  type Env = Map[String, Int]
}
