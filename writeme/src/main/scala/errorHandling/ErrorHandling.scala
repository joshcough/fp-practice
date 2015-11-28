package errorHandling

import let.monadic.LetAST
import LetAST._

/**
  * Created by jcough on 11/27/15.
  */
object ErrorHandling {

  /**
    *
    */
  object LetLangOption {

    def interp(node: Exp, env: Env=Map()): Option[Int] = node match {
      case Num (i)   => ???
      case Add (l,r) => ???
      case Mult(l,r) => ???
      case Var (v)   => ???
      case Let ((x,e),b) => ???
    }
  }

  /**
    *
    */
  object LetLangEither {

    type Result = Either[String, Int]

    implicit class RichEither[E,A](e:Either[E,A]) {
      def map[B](f: A => B): Either[E,B] =
        e.fold(Left(_), (a:A) => Right(f(a)))
      def flatMap[B](f: A => Either[E,B]): Either[E,B] =
        e.fold(Left(_), (a: A) => f(a))
    }

    def interp(node: Exp, env: Env=Map()): Result = node match {
      case Num (i)   => ???
      case Add (l,r) => ???
      case Mult(l,r) => ???
      case Var (v)   => ???
      case Let ((x,e),b) => ???
    }

    def run(node: Exp, expected: Either[String, Int]) = {
      val i = interp(node)
      if(i!=expected) sys.error(s"expected: $expected, but got: $i")
    }
  }

  /**
    *
    */
  object LetLangDisjunction {

    import scalaz.\/
    import scalaz.syntax.either._

    type Result = String \/ Int

    def interp(node: Exp, env: Env=Map()): Result = node match {
      case Num (i)   => ???
      case Add (l,r) => ???
      case Mult(l,r) => ???
      case Var (v)   => ???
      case Let ((x,e),b) => ???
    }
  }

  /**
    *
    */
  object LetLangMonad {

    import scala.language.higherKinds
    import scalaz.Monad
    import scalaz.syntax.monad._

    def lookup(v: String, env: Env): Int =
      env.getOrElse(v, sys.error(s"unbound variable: $v, env: $env"))

    def interp[F[_]](node: Exp, env: Env=Map())
                    (implicit m: Monad[F]): F[Int] =
      node match {
        case Num (i)   => ???
        case Add (l,r) => ???
        case Mult(l,r) => ???
        case Var (v)   => ???
        case Let ((x,e),b) => ???
      }

    def run[F[_]](node: Exp, expected: F[Int])
                 (implicit m: Monad[F]): F[Int] = {
      val i = interp(node)
      if(i!=expected) sys.error(s"expected: $expected, but got: $i")
      i
    }

    def main(args: Array[String]): Unit = {
      import scalaz.Scalaz._
      import scalaz.{Reader, Writer, State, \/}

      // all the common error handling monads
      println(run[Option](Num(6), 6.some))
      type V[A] = String \/ A
      println(run[V](Num(6),      6.right))
      type E[A] = Either[String,A]
      println(run[E](Num(6),      Right(6)))

      // one monad you might be surprised to see here.
      println(run[List](Num(6),   List(6)))

      // some more advanced monads
      type S[A] = State[Int, A]
      println(interp[S](Num(6)).run(0))
      type W[A] = Writer[Int, A]
      println(interp[W](Num(6)).run)
      type R[A] = Reader[Int, A]
      println(interp[R](Num(6)).run(0))

      // this shows that they will all fail in the same way.
      // this is because Monad by itself doesn't do error handling.
      println(run[Option](Var("x"), 6.some))
    }
  }

  /**
    *
    */
  object LetLangMonadError {

    import scala.language.higherKinds
    import scalaz.MonadError
    import scalaz.MonadListen
    import scalaz.syntax.monad._
    import scalaz.syntax.monadError._

    type Env = Map[String, Int]

    implicit class Parser(val sc: StringContext) extends AnyVal {
      def v(args: Any*): Var = Var(sc.parts.mkString)
      def n(args: Any*): Num = Num(sc.parts.mkString.toInt)
    }

    def lookup[F[_,_]](v: String, env: Env)
                      (implicit m: MonadError[F, String]): F[String, Int] =
      env.get(v).fold[F[String, Int]](
        s"unbound variable $v".raiseError[F,Int])((i: Int) => m.point(i))

    def interp[F[_,_]](node: Exp, env: Env=Map())
                      (implicit m: MonadError[F, String]): F[String, Int] =
      node match {
        case Num (i)   => ???
        case Add (l,r) => ???
        case Mult(l,r) => ???
        case Var (v)   => ???
        case Let ((x,e),b) => ???
      }

    def run[F[_,_]](node: Exp, expected: F[String, Int])
                   (implicit m: MonadError[F, String]): F[String, Int] = {
      val i = interp(node)
      if(i!=expected) sys.error(s"expected: $expected, but got: $i")
      i
    }

    def main(args: Array[String]): Unit = {
      import scalaz.{\/,WriterT}
      import scalaz.Scalaz._
      import scalaz.WriterT._
      import scalaz.Writer._
      import scalaz.std.string._
      import scalaz.Id._

      println(run[\/](Num(6), 6.right))
      println(run[\/](Var("x"),     "unbound variable x".left))

      println(run[Either](Num(6),   Right(6)))
      println(run[Either](Var("x"), Left("unbound variable x")))
    }
  }


  //
  //type T[E, A] = MonadError[WriterT[String \/ ?, ?, A] , E]
  //println(interp[T](Num(6)).run)
  // hmmmm
  //(Monoid w, MonadError e m) => MonadError e (WriterT w m)
  //(Monoid w, Monad m) => MonadWriter w (WriterT w m)
  //class (Monoid w, Monad m) => MonadWriter w m | m -> w where
  //newtype WriterT w m a :: * -> (* -> *) -> * -> *
  //runWriterT :: m (a, w)
  // final case class WriterT[F[_], W, A](run: F[(W, A)]) { self =>

}
