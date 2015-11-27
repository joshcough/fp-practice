package answers

/**
  * Start Here
  */
object FirstLang {

  trait Exp
    case class Num(i:Int)         extends Exp
    case class Add (l:Exp, r:Exp) extends Exp
    case class Mult(l:Exp, r:Exp) extends Exp

  /**
    * FILL ME IN
    * @param node
    * @return
    */
  def interp(node: Exp): Int = node match {
    case Num (i)   => i
    case Add (l,r) => interp(l) + interp(r)
    case Mult(l,r) => interp(l) * interp(r)
  }

  implicit class Parser(val sc: StringContext) extends AnyVal {
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