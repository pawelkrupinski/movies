package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.schedule.InMemoryScheduledRunStore
import tools.{LogCapture, ManualScheduler, MutableClock, RecordingTickRecorder, TickRun}

import java.time.Instant
import scala.concurrent.duration.*

class ClaimedEnqueueReaperSpec extends AnyFlatSpec with Matchers {

  private class Rig(timing: ClaimedEnqueueReaper.Timing, at: String) {
    val clock    = new MutableClock(Instant.parse(at))
    var enqueued = 0
    val reaper   = new ClaimedEnqueueReaper("job", () => { enqueued += 1; Enqueued(1, 0) }, 24.hours, 5.minutes,
      new InMemoryScheduledRunStore, clock, timing)
    def at(instant: String): Unit = clock.advance(java.time.Duration.between(clock.instant(), Instant.parse(instant)))
  }

  // The daily share-card prune ran five minutes after whichever deploy came first in the UTC day —
  // 05:03 one day, 00:09 the next — so its burst of deletions wandered with the deploys.
  "A reaper aligned to a time of day" should "enqueue once per window starting at that time, whenever the replicas boot" in {
    val rig = new Rig(ClaimedEnqueueReaper.Timing.Aligned(3.hours), "2026-09-25T03:00:01Z")
    rig.reaper.tickIfClaimed() shouldBe Some(TickRun.Completed)
    // A deploy at 00:04 the next day: its boot tick finds the window that began at 03:00 claimed.
    rig.at("2026-09-26T00:09:00Z")
    rig.reaper.tickIfClaimed() shouldBe None
    rig.at("2026-09-26T03:00:01Z")
    rig.reaper.tickIfClaimed() shouldBe Some(TickRun.Completed)
    rig.enqueued shouldBe 2
  }

  it should "schedule each tick after the boot tick just past the next window's start" in {
    val rig = new Rig(ClaimedEnqueueReaper.Timing.Aligned(3.hours), "2026-09-26T00:09:00Z")
    rig.reaper.nextDelay() shouldBe (2.hours + 51.minutes + ClaimedEnqueueReaper.Timing.Margin)
    rig.at("2026-09-26T03:00:01Z")
    rig.reaper.nextDelay() shouldBe (24.hours - 1.second + ClaimedEnqueueReaper.Timing.Margin)
  }

  "A reaper timed from boot" should "tick every interval after the last" in {
    val rig = new Rig(ClaimedEnqueueReaper.Timing.FromBoot, "2026-09-26T00:09:00Z")
    rig.reaper.nextDelay() shouldBe 24.hours
  }

  // MongoTaskQueue answers a write it could not make with EnqueueResult.Failed and logs it; the reaper dropped the
  // answer, so a queue failing every enqueue still completed every run and its stale alert could never fire.
  "A reaper whose enqueue the queue could not answer" should "not complete its tick, and complete it once the enqueue lands" in {
    val queue     = new FailingEnqueueTaskQueue
    val clock     = new MutableClock(Instant.parse("2026-09-26T00:09:00Z"))
    val scheduler = new ManualScheduler(clock)
    val ticks     = new RecordingTickRecorder
    val reaper    = new ClaimedEnqueueReaper("job", () => Enqueued.of(queue.enqueue(TaskType.AuditReadModelContent, "job",
      submittedAt = clock.instant())), 1.hour, 0.minutes, new InMemoryScheduledRunStore, clock, ticks = ticks, newScheduler = _ => scheduler)
    LogCapture.capture(classOf[ClaimedEnqueueReaper].getName) {
      reaper.start()
      scheduler.advance(java.time.Duration.ZERO)
      ticks.completedRuns shouldBe empty   // the enqueue failed: not a completed run
      queue.failing = false
      scheduler.advance(java.time.Duration.ofHours(1))
      ticks.completedRuns should have size 1
      reaper.stop()
    }
    reaper.tickIfClaimed() shouldBe None   // the window already claimed: a no-op, which completes
  }
}
