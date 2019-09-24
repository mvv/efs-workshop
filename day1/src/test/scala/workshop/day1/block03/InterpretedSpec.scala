package workshop.day1.block03

import org.specs2.mutable.Specification

class InterpretedSpec extends Specification {
  import InterpretedSpec._

  "Interpreted" >> {
    "should be runnable" >> {
      Interpreted(3).flatMap(x => Interpreted(x * 2)).run must beEqualTo(6)
    }

    "flatMap should be stack safe" >> {
      def loop(x: Int): Interpreted[Int] = if (x < Limit) bump(x).flatMap(loop) else Interpreted(x)
      lazy val program = loop(0)
      program must not(throwA[Throwable])
      lazy val result = program.run
      result must not(throwA[Throwable])
      result must beEqualTo(Limit)
    }

    /*
     * PART2
     */

    "Can it handle non-tail recursion?" >> {
      def loop(x: Int): Interpreted[Int] =
        if (x < Limit) bump(x).flatMap(loop).flatMap(x => Interpreted(x)) else Interpreted(x)
      lazy val program = loop(0)
      program must not(throwA[Throwable])
      lazy val result = program.run
      result must throwA[StackOverflowError]
    }

    "It can on the heap" >> {
      def loop(x: Int): Interpreted[Int] =
        if (x < Limit) bump(x).flatMap(loop).flatMap(x => Interpreted(x)) else Interpreted(x)
      lazy val program = loop(0)
      program must not(throwA[Throwable])
      lazy val result = Interpreted.runOnHeap(program)
      result must not(throwA[Throwable])
      result must beEqualTo(Limit)
    }
  }
}

object InterpretedSpec {
  val Limit = 100000
  def bump(x: Int): Interpreted[Int] = Interpreted(x + 1)
}
