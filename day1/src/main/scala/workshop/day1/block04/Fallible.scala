package workshop.day1.block04

import cats.MonadError

sealed trait Fallible[+A] {
  final def flatMap[B](f: A => Fallible[B]): Fallible[B] = Fallible.FlatMap(this, f)
  final def fold[B](success: A => Fallible[B], failure: Throwable => Fallible[B]): Fallible[B] =
    Fallible.Fold(this, success, failure)
  final def ensuring(cleanup: Fallible[Unit]): Fallible[A] = ???
}

object Fallible {
  type M[+A] = Fallible[A]

  private final case class Pure[+A](value: A) extends M[A]
  private final case class Fail(cause: Throwable) extends M[Nothing]
  private final case class Effect[+A](value: () => A) extends M[A]
  private final case class Fold[A, +B](action: M[A], success: A => M[B], failure: Throwable => M[B]) extends M[B]
  private final case class FlatMap[A, +B](action: M[A], chain: A => M[B]) extends M[B]

  def succeed[A](value: A): M[A] = Pure(value)
  def fail(cause: Throwable): M[Nothing] = Fail(cause)
  def effect[A](value: => A): M[A] = Effect(() => value)

  def run[A](action: M[A]): Either[Throwable, A] = ???

  implicit val fallibleMonad: MonadError[Fallible, Throwable] = new MonadError[Fallible, Throwable] {
    override def pure[A](x: A): Fallible[A] = succeed(x)
    override def flatMap[A, B](fa: Fallible[A])(f: A => Fallible[B]): Fallible[B] = fa.flatMap(f)
    override def tailRecM[A, B](a: A)(f: A => Fallible[Either[A, B]]): Fallible[B] = f(a).flatMap {
      case Left(a1)      => tailRecM(a1)(f)
      case Right(result) => succeed(result)
    }
    override def raiseError[A](e: Throwable): Fallible[A] = fail(e)
    override def handleErrorWith[A](fa: Fallible[A])(f: Throwable => Fallible[A]): Fallible[A] = ???
  }
}
