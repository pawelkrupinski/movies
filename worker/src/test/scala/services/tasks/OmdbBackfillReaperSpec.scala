package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.schedule.{InMemoryScheduledRunStore, NeverClaimScheduledRunStore}

import scala.concurrent.duration._

class OmdbBackfillReaperSpec extends AnyFlatSpec with Matchers {

  "OmdbBackfillReaper.tickIfClaimed" should "run the backfill when it wins the window's claim" in {
    var sweeps = 0
    new OmdbBackfillReaper(() => sweeps += 1, runStore = new InMemoryScheduledRunStore).tickIfClaimed() shouldBe true
    sweeps shouldBe 1
  }

  it should "skip the backfill when another machine has claimed the window" in {
    var sweeps = 0
    new OmdbBackfillReaper(() => sweeps += 1, runStore = NeverClaimScheduledRunStore).tickIfClaimed() shouldBe false
    sweeps shouldBe 0
  }

  it should "sweep at most once per window — a second tick in the same window is a no-op" in {
    var sweeps = 0
    val r = new OmdbBackfillReaper(() => sweeps += 1, interval = 24.hours, runStore = new InMemoryScheduledRunStore)
    r.tickIfClaimed() shouldBe true
    r.tickIfClaimed() shouldBe false // same 24h window already claimed by this machine
    sweeps shouldBe 1
  }
}
