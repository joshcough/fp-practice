package print

object LetAndPrint {

  trait Exp
    case class Num  (i:Int)                      extends Exp
    case class Add  (l:Exp, r:Exp)               extends Exp
    case class Mult (l:Exp, r:Exp)               extends Exp
    case class Var  (v: String)                  extends Exp
    case class Let  (v: (String, Exp), body:Exp) extends Exp
    case class Print(e: Exp)                     extends Exp

  type Env    = Map[String, Int]
  type Output = List[String]

  def lookup(v: String, env: Env, output: Output): Int =
    env.getOrElse(v, sys.error(
      s"unbound variable, env: $env, output: ${output.mkString("\n")}"
    ))

  def eval(exp: Exp, env: Env=Map(),
             output: Output=List()): (Output, Int) =
    exp match {
      case Num (i)       => (output, i)
      case Add (l,r)     =>
        val (lo,lv) = eval(l,env,output)
        val (ro,rv) = eval(r,env,lo)
        (ro,lv+rv)
      case Mult(l,r)     =>
        val (lo,lv) = eval(l,env,output)
        val (ro,rv) = eval(r,env,lo)
        (ro,lv*rv)
      case Var (x)       => (output, lookup(x, env, output))
      case Let ((x,e),b) =>
        val (nextOutput,eValue) = eval(e, env, output)
        eval(b, env + (x -> eValue), nextOutput)
      case Print(e)      =>
        val (nextOutput,eValue) = eval(e, env, output)
        (nextOutput ++ List(eValue.toString), eValue)
    }
}
