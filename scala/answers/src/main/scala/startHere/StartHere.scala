package startHere

/**
  * FirstLang contains a very simple arithmetic expression "language",
  * and an interpreter for it. The interpreter function takes
  * an Exp (expression) as an argument, and evaluates it to its
  * final value. For example: (5 + 6) * (2 + 3) ==> 55
  */
object FirstLang {

  // the language / abstract syntax tree
  trait Exp
    case class Num(i:Int)          extends Exp
    case class Add (l:Exp, r:Exp)  extends Exp
    case class Mult(l:Exp, r:Exp)  extends Exp

  // the interpreter
  def eval(exp: Exp): Int = exp match {
    case Num (i)   => i
    case Add (l,r) => eval(l) + eval(r)
    case Mult(l,r) => eval(l) * eval(r)
  }
}

/**
  * SecondLang adds two expressions:
  *
  * - Eq (aka ==)
  * - If (if p then t else f)
  *
  * These expressions will expose some flaws in our language.
  * Mainly, since we only have integers, what ones are true or false?
  * We'll see that this is a completely arbitrary decision.
  */
object SecondLang {

  trait Exp
    case class Num(i:Int)                extends Exp
    case class Add (l:Exp, r:Exp)        extends Exp
    case class Mult(l:Exp, r:Exp)        extends Exp
    case class Eq  (l:Exp, r:Exp)        extends Exp
    case class If  (p:Exp, t:Exp, f:Exp) extends Exp

  def eval(exp: Exp): Int = exp match {
    case Num (i)   => i
    case Add (l,r) => eval(l) + eval(r)
    case Mult(l,r) => eval(l) * eval(r)
    case Eq  (l,r) =>
      // Here, we make an arbitrary decision to return 1 for true
      if(eval(l) == eval(r)) 1 else 0
    case If(predicate,tBranch,fBranch) =>
      // Again, an arbitrary decision that only 1 is true,
      // and that all other values are false.
      if(eval(predicate) == 1) eval(tBranch) else eval(fBranch)
  }
}

/**
  * ThirdLang has the exact same AST as SecondLang, but the
  * interpreter differs dramatically.
  *
  * Here, we introduce a new data type (booleans), so that our expressions
  * no longer always return integers. In order to do this, we'll
  * have to change the semantics for the Eq and If expressions.
  *
  * Important note:
  * While adding data types is fun, and we'll definitely return to it,
  * after ThirdLang, we'll revert to just using integers
  * for quite some time. Extra data types will be very useful later,
  * but for quite some time they are just extra noise. You'll
  * be surprised how much we can learn about programming languages
  * with just integers!
  */
object ThirdLang {

  trait Exp
    case class Num(i:Int)          extends Exp
    case class Add (l:Exp, r:Exp)  extends Exp
    case class Mult(l:Exp, r:Exp)  extends Exp
    case class Eq  (l:Exp, r: Exp) extends Exp
    case class If  (p:Exp, t:Exp, f:Exp) extends Exp

  trait RuntimeValue
    case class NumV(i:Int) extends RuntimeValue
    trait BooleanValue extends RuntimeValue
      case object True  extends BooleanValue
      case object False extends BooleanValue

  def eval(exp: Exp): RuntimeValue = exp match {
    case Num (i)   => NumV(i)
    case Add (l,r) => numOp(l,r)(_+_)
    case Mult(l,r) => numOp(l,r)(_*_)
    case Eq  (l,r) => boolOp(l,r)
    case If(p,t,f) => ifOp(p,t,f)
  }

  def numOp(l:Exp, r:Exp)(f: (Int, Int) => Int): RuntimeValue =
    (eval(l), eval(r)) match {
      case (NumV(lv), NumV(rv)) => NumV(f(lv,rv))
      case bad => sys.error(s"can't add: $bad")
    }

  /*
   * Now that we no longer have to return an arbitrary int,
   * we can return a BoolV (which represents a Boolean value)
   *
   * There are still arbitrary decisions we need to make though.
   * What happens if we try to compare a number to a boolean? Eg. (5 == true)
   *
   * Should that be legal?
   *  - If so, which integers are equal to true, and which false?
   *  - If not, we need to throw a runtime error if this ever happens.
   *
   * Here, we choose to make that illegal, as it's slightly simpler.
   * Later, when we talk about type checkers, we could turn this
   * runtime error into a compile time error.
   */
  def boolOp(l:Exp, r:Exp): RuntimeValue =
    (eval(l), eval(r)) match {
      case (lv:BooleanValue, rv:BooleanValue) =>
        if(lv == rv) True else False
      case (NumV(lv), NumV(rv)) =>
        if(lv == rv) True else False
      case bad => sys.error(s"can't add: $bad")
    }

  /*
   * If is also much easier now that we have Booleans built in.
   * Instead of making a completely arbitrary decision that only 1 is true,
   * we _know_ what true and false are - they are just primitives!
   */
  def ifOp(predicate: Exp,
           trueBranch: Exp, falseBranch: Exp): RuntimeValue = {
    // This decision is _much_ easier than in SecondLang!
    if(eval(predicate) == True)
      eval(trueBranch)
    else
      eval(falseBranch)
  }
}