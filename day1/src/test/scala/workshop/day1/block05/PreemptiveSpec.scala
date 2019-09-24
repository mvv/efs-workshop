package workshop.day1.block05

import java.util.concurrent.Executors

import org.specs2.concurrent.ExecutionEnv
import org.specs2.mutable.Specification
import sash._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class PreemptiveSpec(implicit ee: ExecutionEnv) extends Specification {
  "Preemptive" >> {
    "should run finalizers" >> {
      var wasPastFailure = false
      var wasInHandler = false
      var wasPastInternalError = false
      var wasInInnerFinalizer = false
      var wasInOuterFinalizer = false
      var wasPastInnerFinalizer = false
      var wasPastOuterFinalizer = false
      val program = effect[Throwable, Unit] {
        try {
          try {
            Preemptive.succeed("foo")
            Preemptive.effect {
              throw new RuntimeException
            }
            Preemptive.effectTotal { wasPastFailure = true }
          } catch {
            case _: RuntimeException =>
              Preemptive.effectTotal { wasInHandler = true }
              Preemptive.unit.flatMap(_ => throw new RuntimeException("internal"))
              Preemptive.effectTotal { wasPastInternalError = true }
          } finally {
            Preemptive.effectTotal { wasInInnerFinalizer = true }
          }
          Preemptive.effectTotal { wasPastInnerFinalizer = true }
        } finally {
          Preemptive.effectTotal { wasInOuterFinalizer = true }
        }
        Preemptive.effect { wasPastOuterFinalizer = true }
      }
      Preemptive.run(program) must beEqualTo(like[Exit[Throwable, Unit]] {
        case Exit.Death(cause: RuntimeException) if cause.getMessage == "internal" => ok
      }).await(retries = 5, timeout = 1.second)
      wasPastFailure must beFalse
      wasInHandler must beTrue
      wasPastInternalError must beFalse
      wasInInnerFinalizer must beTrue
      wasPastInnerFinalizer must beFalse
      wasInOuterFinalizer must beTrue
      wasPastOuterFinalizer must beFalse
    }

    "should run fibers concurrently" >> {
      val NumFibers = 100000
      var numRunning = 0
      var maxNumRunning = 0
      def nop(numInstr: Int): Preemptive[Nothing, Unit] = effect {
        if (numInstr > 0) {
          Preemptive.effectTotal(())
          nop(numInstr - 1)
        } else {
          Preemptive.unit
        }
      }
      val fiberProgram = effect {
        Preemptive.effectTotal {
          numRunning += 1
          if (numRunning > maxNumRunning) {
            maxNumRunning = numRunning
          }
          numRunning
        }
        nop(105)
        Preemptive.effectTotal {
          numRunning -= 1
        }
      }
      def loop(left: Int): Preemptive[Nothing, Unit] = effect {
        if (left > 0) {
          fiberProgram.fork
          loop(left - 1)
        } else {
          Preemptive.unit
        }
      }
      val program = loop(NumFibers)
      val ec = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())
      Preemptive.run(program)(ec) must beEqualTo(()).await(retries = 60, timeout = 1.second)
      eventually(retries = 60, sleep = 1.second) {
        numRunning must be_==(0)
      }
      maxNumRunning must be_>(1)
    }
  }
}
