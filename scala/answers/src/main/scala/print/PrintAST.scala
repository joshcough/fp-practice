package print

/**
  * TODO: add statement blocks here...
  * Created by jcough on 12/23/15.
  */
object PrintAST {
  trait Exp
    case class Num  (i:Int)               extends Exp
    case class Add  (l:Exp, r:Exp)        extends Exp
    case class Mult (l:Exp, r:Exp)        extends Exp
    case class Eq   (l:Exp, r:Exp)        extends Exp
    case class If   (p:Exp, t:Exp, f:Exp) extends Exp
    case class Print(e: Exp)              extends Exp

  type Output = List[String]

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  implicit class RichExp(e:Exp) {
    def +(e2: Exp) = Add(e, e2)
    def *(e2: Exp) = Mult(e, e2)
    def mustBe(printed: List[Int], value: Int) = (e, printed, value)
  }

  implicit class RichInt(i:Int) {
    def n = Num(i)
  }
}
