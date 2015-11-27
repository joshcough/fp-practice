package answers.print

object LetAndPrintStatementBlocks {

  trait Exp
    case class Num  (i:Int)                      extends Exp
    case class Add  (l:Exp, r:Exp)               extends Exp
    case class Mult (l:Exp, r:Exp)               extends Exp
    case class Var  (v: String)                  extends Exp
    case class Let  (v: (String, Exp), body:Exp) extends Exp
    case class Print(e: Exp)                     extends Exp
    case class Statements(es:List[Exp])          extends Exp

  type Env    = Map[String, Int]
  type Output = List[String]

  def die[A](msg: String, env: Env, out: Output): A =
    sys.error(s"error: $msg, env: $env, output: ${out.mkString("\n")}")

  def lookup(v: String, env: Env, output: Output): Int =
    env.getOrElse(v, die(s"unbound variable: $v", env, output))

  def interp(node: Exp,
             env: Env=Map(),
             output: Output=List()): (Output, Int)  =
    node match {
      case Num (i)        => (output, i)
      case Add (l,r)      =>
        val (lo,lv) = interp(l,env,output)
        val (ro,rv) = interp(r,env,lo)
        (ro,lv+rv)
      case Mult(l,r)      =>
        val (lo,lv) = interp(l,env,output)
        val (ro,rv) = interp(r,env,lo)
        (ro,lv*rv)
      case Var (x)        => (output, lookup(x, env, output))
      case Let ((x,e),b)  =>
        val (nextOutput,eValue) = interp(e, env, output)
        interp(b, env + (x -> eValue), nextOutput)
      case Print(e)       =>
        val (nextOutput,eValue) = interp(e, env, output)
        (nextOutput ++ List(eValue.toString), 0)
      case Statements(es) =>
        es.foldLeft((output,0)){ case ((outacc,_),e) =>
          interp(e, env, outacc)
        }
    }

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
