package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.schedule.{InMemoryScheduledRunStore, NeverClaimScheduledRunStore}

import scala.concurrent.duration._

class OmdbBackfillReaperSpec extends AnyFlatSpec with Matchers {

  "OmdbBackfillReaper.tickIfClaimed" should "run the backfill when it wins the window's claim" in {
    var sweeps = 0
    new OmdbBackfillReaper(() => { sweeps += 1; Enqueued(1, 0) }, runStore = new InMemoryScheduledRunStore, clock = _root_.tools.SpecClock.Pinned).tickIfClaimed() shouldBe Some(tools.TickRun.Completed)
    sweeps shouldBe 1
  }

  it should "skip the backfill when another machine has claimed the window" in {
    var sweeps = 0
    new OmdbBackfillReaper(() => { sweeps += 1; Enqueued(1, 0) }, runStore = NeverClaimScheduledRunStore, clock = _root_.tools.SpecClock.Pinned).tickIfClaimed() shouldBe None
    sweeps shouldBe 0
  }

  it should "sweep at most once per window — a second tick in the same window is a no-op" in {
    var sweeps = 0
    val r = new OmdbBackfillReaper(() => { sweeps += 1; Enqueued(1, 0) }, interval = settings.OmdbBackfillInterval(24.hours), runStore = new InMemoryScheduledRunStore, clock = _root_.tools.SpecClock.Pinned)
    r.tickIfClaimed() shouldBe Some(tools.TickRun.Completed)
    r.tickIfClaimed() shouldBe None // same 24h window already claimed by this machine
    sweeps shouldBe 1
  }
}
