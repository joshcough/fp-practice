package answers.let.monadic

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

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  implicit class RichExp(e:Exp) {
    def +(e2: Exp) = Add(e, e2)
    def *(e2: Exp) = Mult(e, e2)
    def shouldBe(i:Int) = (e,i)
  }

  implicit class RichInt(i:Int) {
    def n = Num(i)
  }
}
