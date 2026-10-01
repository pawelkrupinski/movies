package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.ReadModelProjector

import scala.concurrent.duration._

/**
 * The hand-off watermark a prune sweep's heal verdict waits on (`ReadModelProjector.awaitStreamApplied`):
 * every event handed to the apply thread up to a ticket has run, across all three cursors — and an
 * event handed off AFTER the mark does not hold the wait, or a busy stream would hold it every time.
 */
class ChangeStreamLivenessSpec extends AnyFlatSpec with Matchers {

  "appliedThrough" should "hold until every event handed off up to the mark has been applied, on any cursor" in {
    val liveness = new ChangeStreamLiveness()
    liveness.appliedThrough(liveness.lastTicket) shouldBe true               // nothing ever handed off
    val slot   = liveness.queued(ChangeStreamLiveness.Slots)
    val movies = liveness.queued(ChangeStreamLiveness.Movies)
    val mark   = liveness.lastTicket
    val later  = liveness.queued(ChangeStreamLiveness.Screenings)          // handed off after the mark

    liveness.appliedThrough(mark) shouldBe false
    liveness.applied(ChangeStreamLiveness.Movies, movies)
    liveness.appliedThrough(mark) shouldBe false                            // the slot event still waits
    liveness.applied(ChangeStreamLiveness.Slots, slot)
    withClue("an event handed off after the mark must not hold it: ") { liveness.appliedThrough(mark) shouldBe true }
    liveness.appliedThrough(later) shouldBe false
  }

  "awaitStreamApplied" should "not wait at all when no cursor ever subscribed" in {
    val started = System.nanoTime()
    ReadModelProjector.awaitStreamApplied(new ChangeStreamLiveness(), grace = 10.seconds, timeout = 10.seconds)
    (System.nanoTime() - started).nanos should be < 5.seconds
  }

  it should "give up at its timeout on an apply that never runs, rather than hold the sweep" in {
    val liveness = new ChangeStreamLiveness()
    liveness.watching(ChangeStreamLiveness.Movies)
    liveness.queued(ChangeStreamLiveness.Movies)                             // never applied
    val started = System.nanoTime()
    ReadModelProjector.awaitStreamApplied(liveness, grace = Duration.Zero, timeout = 200.millis)
    (System.nanoTime() - started).nanos should (be >= 200.millis and be < 5.seconds)
  }

  it should "wait past the longest a debounce holds a film's re-read, so a held film is not called a miss" in {
    ReadModelProjector.InFlightApplyTimeout should be > MovieChangeStream.Debounce.Worker.cap
  }

  "appliedThrough" should "hold for a re-read opened up to the mark until it is over, held by a debounce or not" in {
    val liveness = new ChangeStreamLiveness()
    val held     = liveness.reread()
    val mark     = liveness.lastTicket
    liveness.appliedThrough(mark) shouldBe false
    liveness.pendingApplies(ChangeStreamLiveness.Slots) shouldBe 0          // a hold is not apply lag
    liveness.finished(held)
    liveness.appliedThrough(mark) shouldBe true
  }
}
