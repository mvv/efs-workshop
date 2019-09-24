package workshop.day1.block02

final case class Deferred[+A](value: () => A) {
  def run: A = ???
  def flatMap[B](f: A => Deferred[B]): Deferred[B] = ???
}

object Deferred {
  def lift[A](value: => A): Deferred[A] = ???
}
