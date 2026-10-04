package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ChangeStreamLiveness

import java.time.{Clock, Instant, ZoneOffset}
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration._

/** The wait between fixpoint passes for the change streams to go quiet. The liveness it reads runs
 *  on its owner's clock, which in every harness is pinned (or moved only by the test) — and a
 *  pinned clock's stamp sequence advances a millisecond per reading, so a quiet window measured on
 *  it never elapsed: every convergence leg's next-day test timed out on it (run 37172281810). */
class FixpointPassQuietSpec extends AnyFlatSpec with Matchers {

  private def pinnedLiveness(): ChangeStreamLiveness = {
    val liveness = new ChangeStreamLiveness(Clock.fixed(Instant.parse("2026-08-02T00:00:00Z"), ZoneOffset.UTC))
    liveness.watching(ChangeStreamLiveness.Movies)
    liveness
  }

  "awaitQuiet" should "settle once nothing was delivered for the quiet window, though the liveness's clock is pinned" in {
    val liveness = pinnedLiveness()
    liveness.delivered(ChangeStreamLiveness.Movies)
    noException should be thrownBy
      FixpointPass.awaitQuiet(liveness, () => 0, () => (), quiet = 300.millis, within = 5.seconds)
  }

  it should "not settle while a cursor keeps delivering" in {
    val liveness   = pinnedLiveness()
    val delivering = new AtomicBoolean(true)
    val cursor     = new Thread(() => while (delivering.get()) { liveness.delivered(ChangeStreamLiveness.Movies); Thread.onSpinWait() })
    cursor.setDaemon(true)
    cursor.start()
    try {
      the[IllegalStateException] thrownBy
        FixpointPass.awaitQuiet(liveness, () => 0, () => (), quiet = 300.millis, within = 1.second) should
        have message "the change streams (movies) were still delivering or applying 1 second after the pass — " +
          "a pass that never stops writing is churn in its own right"
    } finally delivering.set(false)
  }

  it should "not settle while an apply is pending" in {
    val liveness = pinnedLiveness()
    liveness.queued(ChangeStreamLiveness.Movies)
    an[IllegalStateException] should be thrownBy
      FixpointPass.awaitQuiet(liveness, () => 0, () => (), quiet = 100.millis, within = 1.second)
  }
}
