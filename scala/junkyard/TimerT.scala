package everything

import scala.language.higherKinds
import scalaz.{MonadTrans, Monad}
import scalaz.syntax.monad._


object TimerT {

  case class Timer[A](
    startTime:Long = System.currentTimeMillis,
    totalTime:Long = 0, a:A)

  final case class TimerT[M[_], A](run: M[Timer[A]])

  implicit val TimerMonad: Monad[Timer] = new Monad[Timer] {
    def bind[A, B](t1: Timer[A])(f: A => Timer[B]): Timer[B] =
      f(t1.a).copy(totalTime = System.currentTimeMillis - t1.startTime)
    def point[A](a: => A): Timer[A] = Timer(a=a)
  }

  implicit def TimerTMonad[F[_]](implicit F: Monad[F]): Monad[TimerT[F,?]] =
    new Monad[TimerT[F,?]] {
      def point[A](a: => A): TimerT[F, A] = TimerT(F.point(Timer(a=a)))
      def bind[A, B](tfa: TimerT[F, A])(f: A => TimerT[F, B]): TimerT[F, B] =
        TimerT(tfa.run flatMap { ta => f(ta.a).run })
    }

  implicit val TimerMonadTrans: MonadTrans[TimerT] = new MonadTrans[TimerT] {
    def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]): TimerT[G, A] =
      TimerT(G.map(ga)(a => Timer(a=a)))
    implicit def apply[F[_]](implicit G: Monad[F]): Monad[TimerT[F, ?]] =
      TimerTMonad
  }
}
