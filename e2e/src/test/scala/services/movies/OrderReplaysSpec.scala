package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A recording's replays fetch live, so beside the boot they quadrupled its TMDB rate and drew 429s until the breaker
 *  opened and the replays resolved no film (Germany, recording run 37719613743). */
class OrderReplaysSpec extends AnyFlatSpec with Matchers {

  "the order replays" should "start beside the boot only in a hermetic run that includes their test" in {
    OrderReplays.besideTheBoot(hermetic = true, included = true) shouldBe true
    OrderReplays.besideTheBoot(hermetic = false, included = true) shouldBe false
    OrderReplays.besideTheBoot(hermetic = true, included = false) shouldBe false
  }

  // A run whose order test timed out, or never ran, reached `afterAll` with the replays still running: it wrote the
  // refetch list while they still named gaps, and closed the databases under them. Settled first, it waits for them —
  // bounded — and a replay that failed is the order test's to report, never the teardown's.
  "settling the order replays" should "wait for running replays, bounded, and swallow their failure" in {
    val release = new java.util.concurrent.CountDownLatch(1)
    val done    = new java.util.concurrent.atomic.AtomicBoolean(false)
    val running = tools.Alongside.start("held-replays") { release.await(); done.set(true) }
    // still running at its bound: the teardown goes on without them, and says nothing
    OrderReplays.settle(Some(running), scala.concurrent.duration.Duration.Zero)
    done.get shouldBe false
    release.countDown()
    OrderReplays.settle(Some(running), tools.SpecTimeouts.Io)
    done.get shouldBe true
    OrderReplays.settle(Some(tools.Alongside.start("failing-replays")(throw new IllegalStateException("diverged"))), tools.SpecTimeouts.Io)
    OrderReplays.settle(None, tools.SpecTimeouts.Io)
  }
}
