package letAndPrint

object LetAndPrint {

  trait Exp
    case class Num  (i:Int)                      extends Exp
    case class Add  (l:Exp, r:Exp)               extends Exp
    case class Mult (l:Exp, r:Exp)               extends Exp
    case class Eq   (l:Exp, r:Exp)               extends Exp
    case class If   (p:Exp, t:Exp, f:Exp)        extends Exp
    case class Var  (v: String)                  extends Exp
    case class Let  (v: (String, Exp), body:Exp) extends Exp
    case class Print(e: Exp)                     extends Exp
    case class Statements(es:List[Exp]) extends Exp

  type Env    = Map[String, Int]
  type Output = List[String]

  trait Interpreter {
    def interpret(e:Exp): (Output, Int)
  }

  implicit class Parser(val sc: StringContext) extends AnyVal {
    def v(args: Any*): Var = Var(sc.parts.mkString)
    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
  }

  implicit class RichExp(e:Exp) {
    def +(e2: Exp) = Add(e, e2)
    def *(e2: Exp) = Mult(e, e2)
    def mustBe(printed: List[Int], value: Int) = (e, printed, value)
  }

  implicit class RichWhat(t: (String, Exp)) {
    def in(e:Exp) = Let(t,e)
  }

  def print(e:Exp) = Print(e)

  implicit class RichInt(i:Int) {
    def n = Num(i)
  }

  def die[A](msg: String, env: Env): A = sys.error(s"error: $msg, env: $env")
}
