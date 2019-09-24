package workshop.day1.block03

import scala.annotation.tailrec

sealed trait Interpreted[+A] {
  final def flatMap[B](f: A => Interpreted[B]): Interpreted[B] = ???
  final def run: A = Interpreted.run(this)
}

object Interpreted {
  final case class Pure[+A](value: A) extends Interpreted[A]
  final case class FlatMap[A, +B](interpreted: Interpreted[A], chain: A => Interpreted[B]) extends Interpreted[B]

  def apply[A](value: A): Interpreted[A] = Pure(value)

  //@tailrec
  def run[A](interpreted: Interpreted[A]): A = ???

  /*
   * PART2
   */

  def runOnHeap[A](interpreted: Interpreted[A]): A = ???
}
