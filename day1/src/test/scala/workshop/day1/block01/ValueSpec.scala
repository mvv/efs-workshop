package workshop.day1.block01

import org.specs2.mutable.Specification

class ValueSpec extends Specification {
  "Value" >> {
    "flatMap should chain Values" >> {
      Value(3).flatMap(x => Value(x * 2)) must beEqualTo(Value(6))
    }

    "Is flatMap stack-safe?" >> {
      val Limit = 100000
      def bump(x: Int): Value[Int] = Value(x + 1)
      def loop(x: Int): Value[Int] = if (x < Limit) bump(x).flatMap(loop) else Value(x)
      loop(0) must throwA[StackOverflowError]
    }
  }
}
