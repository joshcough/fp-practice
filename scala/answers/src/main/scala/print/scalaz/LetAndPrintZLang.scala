//package answers
//
//import scalaz.Kleisli._
//import scalaz.ReaderT
//import scalaz.Scalaz._
//
//// With Writer Monad
//object LetAndPrintZLang {
//
//  import scalaz.Writer
//  import scalaz.std.list._
//  import scalaz.syntax.writer._
//  import scalaz.syntax.applicative._
//  import scalaz.syntax.foldable._
//
//  trait Exp
//    case class Num  (i:Int)                      extends Exp
//    case class Add  (l:Exp, r:Exp)               extends Exp
//    case class Mult (l:Exp, r:Exp)               extends Exp
//    case class Var  (v: String)                  extends Exp
//    case class Let  (v: (String, Exp), body:Exp) extends Exp
//    case class Print(e: Exp)                     extends Exp
//    case class Statements(es:List[Exp])          extends Exp
//
//  type Env    = Map[String, Int]
//  type Output = List[String]
//
//  implicit class Parser(val sc: StringContext) extends AnyVal {
//    def v(args: Any*): Var = Var(sc.parts.mkString)
//    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
//  }
//
//  type W[A] = Writer[Output, A]
//
//  def die[A](msg: String, env: Env): A = sys.error(s"error: $msg, env: $env")
//
//  def lookup(v: String, env: Env): W[Int] =
//    env.getOrElse(v, die(s"unbound variable: $v", env)).pure[W]
//
//  def interp(exp: Exp, env: Env=Map()): W[Int]  =
//    exp match {
//      case Num (i)        => i.pure[W]
//      case Add (l,r)      => for {
//        lv <- interp(l,env)
//        rv <- interp(l,env)
//      } yield lv+rv
//      case Mult(l,r)      => for {
//        lv <- interp(l,env)
//        rv <- interp(l,env)
//      } yield lv*rv
//      case Var (x)        => lookup(x, env)
//      case Let ((x,e),b)  => for {
//        ev <- interp(e, env)
//        bv <- interp(b, env + (x -> ev))
//      } yield bv
//      case Print(e)       => for {
//        ev <- interp(e, env)
//        _  <- 0.set(List(ev.toString))
//      } yield 0
//      case Statements(es) => es.foldlM(0)(_ => e => interp(e, env))
//    }
//
//  def run(exp: Exp, expected: Int) = {
//    val (out,i) = interp(exp).run
//    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
//  }
//}
//
//object LetAndPrintLangReaderWriter {
//
//  import scalaz.Writer
//  import scalaz.std.list._
//  import scalaz.syntax.writer._
//  import scalaz.syntax.applicative._
//  import scalaz.syntax.foldable._
//
//  trait Exp
//  case class Num  (i:Int)                      extends Exp
//  case class Add  (l:Exp, r:Exp)               extends Exp
//  case class Mult (l:Exp, r:Exp)               extends Exp
//  case class Var  (v: String)                  extends Exp
//  case class Let  (v: (String, Exp), body:Exp) extends Exp
//  case class Print(e: Exp)                     extends Exp
//  case class Statements(es:List[Exp])          extends Exp
//
//  implicit class Parser(val sc: StringContext) extends AnyVal {
//    def v(args: Any*): Var = Var(sc.parts.mkString)
//    def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
//  }
//
//  type Env    = Map[String, Int]
//  type Output = List[String]
//  type W[A] = Writer[Output, A]
//  type R[A] = ReaderT[W, Env, A]
//
//  def die[A](msg: String, env: Env): A = sys.error(s"error: $msg, env: $env")
//
//  def lookup(v: String, env: Env): R[Int] = ???
//    ///env.getOrElse(v, die(s"unbound variable: $v", env)).pure[W]
//
//  def interp(exp: Exp): R[Int]  =
//    exp match {
//      case Num (i)        => i.pure[R]
//      case Add (l,r)      => for {
//        lv <- interp(l)
//        rv <- interp(l)
//      } yield lv+rv
//      case Mult(l,r)      => for {
//        lv <- interp(l)
//        rv <- interp(l)
//      } yield lv*rv
//      case Var (x)        => ??? ///lookup(x)
////      case Let ((x,e),b)  => for {
////        env <- ask[Id,Env]
////        ev  <- interp(e)
////        //bv  <- local((env: Env) => env + (x -> ev))(interp(b))
////      } yield ev // TODO: bad
//      case Print(e)       => for {
//        ev <- interp(e)
//        _  <- 0.set(List(ev.toString))
//      } yield 0
//      case Statements(es) => ??? //es.foldlM(0)(interp)
//    }
//
////  def run(exp: Exp, expected: Int) = {
////    val (out,i) = interp(exp).run
////    if(i!=expected) sys.error(s"expected: $expected, but got: $i")
////  }
//}
