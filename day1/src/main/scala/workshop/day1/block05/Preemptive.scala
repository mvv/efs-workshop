package workshop.day1.block05

import scala.concurrent.{ExecutionContext, Future}

sealed trait Preemptive[+E, +A] {
  final def flatMap[E1 >: E, B](f: A => Preemptive[E1, B]): Preemptive[E1, B] = Preemptive.FlatMap(this, f)
  final def map[B](f: A => B): Preemptive[E, B] = flatMap(f.andThen(Preemptive.succeed))
  final def fold[E1, B](success: A => Preemptive[E1, B], failure: E => Preemptive[E1, B]): Preemptive[E1, B] =
    Preemptive.Fold(this, success, failure)
  final def fork: Preemptive[Nothing, Fiber[E, A]] = Preemptive.Fork(this)
  final def catchSome[E1 >: E, B >: A](handler: PartialFunction[E, Preemptive[E1, B]]): Preemptive[E1, B] = ???
  final def ensuring(cleanup: Preemptive[Nothing, Unit]): Preemptive[E, A] = Preemptive.Protect(this, cleanup)
}

sealed trait Fiber[+E, +A]

sealed trait Exit[+E, +A]
object Exit {
  final case class Success[A](result: A) extends Exit[Nothing, A]
  final case class Failure[E](cause: E) extends Exit[E, Nothing]
  final case class Death(cause: Throwable) extends Exit[Nothing, Nothing]
}

object Preemptive {
  type M[+E, +A] = Preemptive[E, A]

  private final case class Pure[+A](value: A) extends M[Nothing, A]
  private final case class Fail[+E](cause: E) extends M[E, Nothing]
  private final case class Die(cause: Throwable) extends M[Nothing, Nothing]
  private final case class Effect[+A](value: () => A) extends M[Throwable, A]
  private final case class Fork[+E, +A](action: M[E, A]) extends M[Nothing, Fiber[E, A]]
  private final case class Fold[E, +E1, A, +B](action: M[E, A], success: A => M[E1, B], failure: E => M[E1, B])
      extends M[E1, B]
  private final case class Protect[+E, +A](action: M[E, A], cleanup: M[Nothing, Unit]) extends M[E, A]
  private final case class FlatMap[E, +E1 >: E, A, +B](action: M[E, A], chain: A => M[E1, B]) extends M[E1, B]

  val unit: M[Nothing, Unit] = Pure(())
  def succeed[A](value: A): M[Nothing, A] = Pure(value)
  def fail[E](cause: E): M[E, Nothing] = Fail(cause)
  def die(cause: Throwable): M[Nothing, Nothing] = Die(cause)
  def die(message: String): M[Nothing, Nothing] = Die(new RuntimeException(message))
  def effect[A](value: => A): M[Throwable, A] = Effect(() => value)
  def effectTotal[A](value: => A): M[Nothing, A] = effect(value).fold(succeed, die)

  def run[E, A](action: M[E, A])(implicit ec: ExecutionContext): Future[Exit[E, A]] = ???
}
