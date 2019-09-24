package workshop.day1.block04

import cats.syntax.functor._
import cats.syntax.apply._
import cats.syntax.applicativeError._
import org.specs2.mutable.Specification
import com.github.mvv.sash.cats._

class FallibleSpec extends Specification {
  "Fallible" >> {
    "should permit recovering from errors" >> {
      var world = 0
      val program = for {
        x <- Fallible.succeed(42)
        y <- (Fallible.effect { println(s"running with world = $world") } *> Fallible
          .effect(x / world)
          .map(y => y * 100))
          .handleErrorWith(_ => Fallible.succeed(-1))
        _ <- Fallible.effect { world += 1 }
      } yield y
      Fallible.run(program) must beRight(-1)
      Fallible.run(program) must beRight(4200)
      Fallible.run(program) must beRight(2100)
    }

    "should permit recovering from errors (Sash syntax)" >> {
      var world = 0
      val program = effect {
        val x = +Fallible.succeed(42)
        val y = + {
          try {
            Fallible.effect { println(s"running (Sash) with world = $world") }
            val y = +Fallible.effect(x / world)
            Fallible.succeed(y * 100)
          } catch {
            case _: Throwable =>
              Fallible.succeed(-1)
          }
        }
        Fallible.effect { world += 1 }
        Fallible.succeed(y)
      }
      Fallible.run(program) must beRight(-1)
      Fallible.run(program) must beRight(4200)
      Fallible.run(program) must beRight(2100)
    }

    "should stop running on errors" >> {
      var wasAfterFailure = false
      var wasInHandler = false
      var wasAfterTry = false
      val program = effect {
        try {
          Fallible.fail(new RuntimeException)
          Fallible.effect { wasAfterFailure = true }
        } catch {
          case _: RuntimeException =>
            Fallible.effect { wasInHandler = true }
        }
        Fallible.effect { wasAfterTry = true }
      }
      Fallible.run(program) must beRight
      wasAfterFailure must beFalse
      wasInHandler must beTrue
      wasAfterTry must beTrue
    }

    "In an impure language errors can be anywhere" >> {
      val program = Fallible.succeed(42).flatMap(_ => throw new RuntimeException("Impure function"))
      Fallible.run(program) must throwA[RuntimeException](message = "Impure function")
    }

    "should handle tail recursion" >> {
      val Limit = 100000
      def bump(x: Int): Fallible[Int] = Fallible.succeed(x + 1)
      def loop(x: Int): Fallible[Int] = effect {
        if (x < Limit) {
          loop(+bump(x))
        } else {
          Fallible.succeed(x)
        }
      }
      lazy val program = loop(0)
      program must not(throwA[Throwable])
      lazy val result = Fallible.run(program)
      result must not(throwA[Throwable])
      result must beRight(Limit)
    }

    "does not replace original failure cause" >> {
      val program =
        Fallible.fail(new RuntimeException("original")).ensuring(Fallible.fail(new RuntimeException("cleanup")))
      Fallible.run(program) must beLeft(like[Throwable] {
        case e: RuntimeException if e.getMessage == "original" => ok
      })
    }
  }
}
