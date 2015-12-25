package startHere

/**
  * Start Here
  * Your job is to fill in the ???s in eval.
  * You can then run the tests for it in sbt like so:
  *
  * > project writeme
  * > testOnly startHere.FirstTests
  */
object FirstLang {

  /**
    * This is the first example of an AST (abstract syntax tree).
    * We are going to be seeing a lot of them here. Most of
    * them will have this basic shape though.
    * Here, Exp stands for Expression. In this language there are
    * three types of expressions:
    *  - Numbers (Num)
    *  - Addition Expressions (Add)
    *  - Multiplication Expressions (Mult)
    */
  trait Exp
    case class Num(i:Int)         extends Exp
    case class Add (l:Exp, r:Exp) extends Exp
    case class Mult(l:Exp, r:Exp) extends Exp

  /**
    * Write your first interpreter.
    * For each of the cases below, you need to return an Int.
    * @param exp The arithmetic expression to interpret.
    * @return an Int that is the final value of the expression.
    *         For example, Add(Num(1),Num(2)) should return 3.
    */
  def eval(exp: Exp): Int = exp match {
    case Num (i)   => ???
    case Add (l,r) => ???
    case Mult(l,r) => ???
  }
}