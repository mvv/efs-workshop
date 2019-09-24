package workshop.day1.block06

import scala.concurrent.{ExecutionContext, Future}

sealed trait IO[+E, +A] {
  final def flatMap[E1 >: E, B](f: A => IO[E1, B]): IO[E1, B] = IO.FlatMap(this, f)
  final def map[B](f: A => B): IO[E, B] = flatMap(f.andThen(IO.succeed))
  final def fold[E1, B](success: A => IO[E1, B], failure: E => IO[E1, B]): IO[E1, B] =
    IO.Fold(this, success, failure)
  final def fork: IO[Nothing, Fiber[E, A]] = IO.Fork(this)
  final def catchSome[E1 >: E, B >: A](handler: PartialFunction[E, IO[E1, B]]): IO[E, B] = ???
  final def ensuring(cleanup: IO[Nothing, Unit]): IO[E, A] = IO.Protect(this, cleanup)
}

sealed trait Fiber[+E, +A] {
  def join: IO[Nothing, Exit[E, A]]
}

sealed trait Exit[+E, +A]
object Exit {
  final case class Success[A](result: A) extends Exit[Nothing, A]
  final case class Failure[E](cause: E) extends Exit[E, Nothing]
  final case class Death(cause: Throwable) extends Exit[Nothing, Nothing]
}

sealed trait Runtime {
  def executionContext: ExecutionContext
}

object IO {
  type M[+E, +A] = IO[E, A]

  private final case class Pure[+A](value: A) extends M[Nothing, A]
  private final case class Fail[+E](cause: E) extends M[E, Nothing]
  private final case class Die(cause: Throwable) extends M[Nothing, Nothing]
  private final case class Effect[+A](value: () => A) extends M[Throwable, A]
  private final case class Fork[+E, +A](action: M[E, A]) extends M[Nothing, Fiber[E, A]]
  private final case class Fold[E, +E1, A, +B](action: M[E, A], success: A => M[E1, B], failure: E => M[E1, B])
      extends M[E1, B]
  private final case class Protect[+E, +A](action: M[E, A], cleanup: M[Nothing, Unit]) extends M[E, A]
  private final case class FlatMap[E, +E1 >: E, A, +B](action: M[E, A], chain: A => M[E1, B]) extends M[E1, B]
  private final case class EffectAsync[+A](register: (Either[Throwable, A] => Boolean) => Unit) extends M[Throwable, A]
  private case object GetRuntime extends M[Nothing, Runtime]

  val unit: M[Nothing, Unit] = Pure(())
  def succeed[A](value: A): M[Nothing, A] = Pure(value)
  def fail[E](cause: E): M[E, Nothing] = Fail(cause)
  def die(cause: Throwable): M[Nothing, Nothing] = Die(cause)
  def die(message: String): M[Nothing, Nothing] = Die(new RuntimeException(message))
  def effect[A](value: => A): M[Throwable, A] = Effect(() => value)
  def effectTotal[A](value: => A): M[Nothing, A] = effect(value).fold(succeed, die)
  def effectAsync[A](register: (Either[Throwable, A] => Boolean) => Unit): M[Throwable, A] = EffectAsync(register)
  def runtime: M[Nothing, Runtime] = GetRuntime
  def fromFuture[A](future: Future[A]): M[Throwable, A] = ???

  def run[E, A](action: M[E, A])(implicit ec: ExecutionContext): Future[Exit[E, A]] = ???
}
