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

    def interp(exp: Exp, env: Env=Map()): Option[Int] =
      exp match {
        case Num (i)       => ???
        case Add (l,r)     => ???
        case Mult(l,r)     => ???
        case Var (v)       => ???
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

    def interp(exp: Exp, env: Env=Map()): Result =
      exp match {
        case Num (i)       => ???
        case Add (l,r)     => ???
        case Mult(l,r)     => ???
        case Var (v)       => ???
        case Let ((x,e),b) => ???
      }
  }

  /**
    *
    */
  object LetLangDisjunction {

    import scalaz.\/
    import scalaz.syntax.either._

    type Result = String \/ Int

    def interp(exp: Exp, env: Env=Map()): Result =
      exp match {
        case Num (i)       => ???
        case Add (l,r)     => ???
        case Mult(l,r)     => ???
        case Var (v)       => ???
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

    def interp[F[_]](exp: Exp, env: Env=Map())
                    (implicit m: Monad[F]): F[Int] =
      exp match {
        case Num (i)       => ???
        case Add (l,r)     => ???
        case Mult(l,r)     => ???
        case Var (v)       => ???
        case Let ((x,e),b) => ???
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

    def interp[F[_,_]](exp: Exp, env: Env=Map())
                      (implicit m: MonadError[F, String]): F[String, Int] =
      exp match {
        case Num (i)   =>     ???
        case Add (l,r) =>     ???
        case Mult(l,r) =>     ???
        case Var (v)   =>     ???
        case Let ((x,e),b) => ???
      }
  }
}
