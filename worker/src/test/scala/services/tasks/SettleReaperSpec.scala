package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.schedule.{InMemoryScheduledRunStore, NeverClaimScheduledRunStore}

import scala.concurrent.duration._

class SettleReaperSpec extends AnyFlatSpec with Matchers {

  "SettleReaper.tickIfClaimed" should "run the settle when it wins the window's claim" in {
    var settles = 0
    new SettleReaper(() => settles += 1, runStore = new InMemoryScheduledRunStore).tickIfClaimed() shouldBe true
    settles shouldBe 1
  }

  it should "skip the settle when another machine has claimed the window" in {
    var settles = 0
    new SettleReaper(() => settles += 1, runStore = NeverClaimScheduledRunStore).tickIfClaimed() shouldBe false
    settles shouldBe 0
  }

  it should "settle at most once per window — a second tick in the same window is a no-op" in {
    var settles = 0
    val r = new SettleReaper(() => settles += 1, interval = 30.minutes, runStore = new InMemoryScheduledRunStore)
    r.tickIfClaimed() shouldBe true
    r.tickIfClaimed() shouldBe false // same 30-min window already claimed by this machine
    settles shouldBe 1
  }

  // The whole-corpus settle and the read-model reconcile BOTH run a full-corpus pass
  // every 30 min. The reconcile fires at boot+~1 min then every 30 (1/31/61/91…), so
  // a 1-min settle delay co-ticked the two heaviest memory passes on the same minute —
  // their combined transient is what OOM'd the worker's 320m heap. The settle's first
  // tick must therefore land mid-window, well clear of every reconcile tick.
  "SettleReaper.DefaultInitialDelay" should "stagger the settle off the read-model reconcile's ~1-min tick" in {
    val reconcileFirstTick = 1.minute
    val interval           = SettleReaper.DefaultInterval
    // Phase gap to the nearest reconcile tick within the repeating window.
    val rawOffset = (SettleReaper.DefaultInitialDelay - reconcileFirstTick).toMinutes
    val gap       = math.min(rawOffset, interval.toMinutes - rawOffset)
    gap should be >= 10L   // never adjacent to a reconcile tick on either side
  }
}
