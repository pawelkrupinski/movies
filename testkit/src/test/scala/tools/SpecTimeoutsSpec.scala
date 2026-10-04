package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration.*

/** A suite that hangs every test must fail inside its CI job, not a minute per test past it. */
class SpecTimeoutsSpec extends AnyFlatSpec with Matchers {
  private val bound   = 60.seconds
  private val running = "pool-1-thread-3-ScalaTest-running-SomeHungSpec"

  "a wait bound" should "be the full one until the suite has run past its deadline, then shrink" in {
    val starts = new ConcurrentHashMap[String, java.lang.Long]()
    SpecTimeouts.boundFor(bound, running, nowNanos = 0L, starts) shouldBe bound
    SpecTimeouts.boundFor(bound, running, SpecTimeouts.SuiteDeadline.toNanos, starts) shouldBe bound
    SpecTimeouts.boundFor(bound, running, SpecTimeouts.SuiteDeadline.toNanos + 1, starts) shouldBe SpecTimeouts.PastDeadline
  }

  it should "count each suite's deadline from its own first wait" in {
    val starts = new ConcurrentHashMap[String, java.lang.Long]()
    SpecTimeouts.boundFor(bound, running, nowNanos = 0L, starts)
    val later = SpecTimeouts.SuiteDeadline.toNanos + 1
    SpecTimeouts.boundFor(bound, "pool-1-thread-4-ScalaTest-running-AnotherSpec", later, starts) shouldBe bound
  }

  it should "stay whole on a thread no suite runs on" in {
    val starts = new ConcurrentHashMap[String, java.lang.Long]()
    SpecTimeouts.boundFor(bound, "spec-worker-1", 0L, starts)
    SpecTimeouts.boundFor(bound, "spec-worker-1", Long.MaxValue / 2, starts) shouldBe bound
  }

  // The deadline reaches a suite only through ScalaTest's naming of the thread running it.
  it should "see the suite this very spec runs on" in {
    Thread.currentThread.getName should include ("ScalaTest-running-")
  }
}
