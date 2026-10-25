package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.schedule.{InMemoryScheduledRunStore, NeverClaimScheduledRunStore}

import scala.concurrent.duration._

class SettleReaperSpec extends AnyFlatSpec with Matchers {
  private val period = settings.SettleInterval(5.minutes)
  private val first  = SettleReaper.InitialDelay(5.minutes)

  "SettleReaper.tickIfClaimed" should "run the settle when it wins the window's claim" in {
    var settles = 0
    new SettleReaper(() => settles += 1, period, first, runStore = new InMemoryScheduledRunStore, clock = _root_.tools.SpecClock.Pinned).tickIfClaimed() shouldBe Some(tools.TickRun.Completed)
    settles shouldBe 1
  }

  it should "skip the settle when another machine has claimed the window" in {
    var settles = 0
    new SettleReaper(() => settles += 1, period, first, runStore = NeverClaimScheduledRunStore, clock = _root_.tools.SpecClock.Pinned).tickIfClaimed() shouldBe None
    settles shouldBe 0
  }

  it should "settle at most once per window — a second tick in the same window is a no-op" in {
    var settles = 0
    val r = new SettleReaper(() => settles += 1, settings.SettleInterval(30.minutes), first, runStore = new InMemoryScheduledRunStore, clock = _root_.tools.SpecClock.Pinned)
    r.tickIfClaimed() shouldBe Some(tools.TickRun.Completed)
    r.tickIfClaimed() shouldBe None // same 30-min window already claimed by this machine
    settles shouldBe 1
  }
}
