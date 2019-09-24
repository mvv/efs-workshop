package workshop.day1.block02

import org.specs2.mutable.Specification

class DeferredSpec extends Specification {
  "Deferred" >> {
    "flatMap should chain Deferreds" >> {
      Deferred.lift(3).flatMap(x => Deferred.lift(x * 2)).run must beEqualTo(6)
    }

    "Is flatMap stack safe?" >> {
      val Limit = 100000
      def bump(x: Int): Deferred[Int] = Deferred.lift(x + 1)
      def loop(x: Int): Deferred[Int] = if (x < Limit) bump(x).flatMap(loop) else Deferred.lift(x)
      lazy val program = loop(0)
      program must not(throwA[Throwable])
      program.run must throwA[StackOverflowError]
    }
  }
}
