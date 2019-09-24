package workshop.day1.block01

final case class Value[+A](value: A) {
  def flatMap[B](f: A => Value[B]): Value[B] = ???
}
