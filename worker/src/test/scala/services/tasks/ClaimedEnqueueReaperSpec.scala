package services.tasks

import services.tasktypes.TaskType

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.schedule.InMemoryScheduledRunStore
import tools.{LogCapture, ManualScheduler, MutableClock, RecordingTickRecorder, SpecThreadPools, TickRun}

import java.time.Instant
import scala.concurrent.duration.*

class ClaimedEnqueueReaperSpec extends AnyFlatSpec with Matchers {

  private class Rig(timing: ClaimedEnqueueReaper.Timing, at: String) {
    val clock    = new MutableClock(Instant.parse(at))
    var enqueued = 0
    val reaper   = new ClaimedEnqueueReaper("job", () => { enqueued += 1; Enqueued(1, 0) }, 24.hours, 5.minutes,
      new InMemoryScheduledRunStore, clock, timing, threadPools = tools.ThreadPools.Unmanaged)
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
      submittedAt = clock.instant())), 1.hour, 0.minutes, new InMemoryScheduledRunStore, clock, ticks = ticks, threadPools = SpecThreadPools.scheduling(scheduler))
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

  // The enrichment and detail reapers recorded their runs under their own names before they rode this schedule, and
  // their stale alerts read those names: the name a run is recorded under is its own, the window claimed its occurrence's.
  "A reaper recorded under its own name" should "record its runs by that name and claim its occurrence's window" in {
    val clock     = new MutableClock(Instant.parse("2026-09-26T00:09:00Z"))
    val scheduler = new ManualScheduler(clock)
    val ticks     = new RecordingTickRecorder
    val windows   = new InMemoryScheduledRunStore
    val reaper    = new ClaimedEnqueueReaper("job", () => Enqueued(1, 0), 1.hour, 0.minutes, windows, clock, ticks = ticks,
      threadPools = SpecThreadPools.scheduling(scheduler), recordedAs = Some("job-loop"))
    reaper.start()
    scheduler.advance(java.time.Duration.ZERO)
    ticks.completedRuns shouldBe Seq("job-loop" -> 1.hour)
    windows.claim(services.schedule.OccurrenceKey.at("job", clock.millis(), 1.hour, 0.seconds)) shouldBe false
    reaper.stop()
  }

  "A reaper not ready yet" should "leave the window unclaimed for a replica that is" in {
    val clock   = new MutableClock(Instant.parse("2026-09-26T00:09:00Z"))
    val windows = new InMemoryScheduledRunStore
    var ready   = false
    var runs    = 0
    def reaper  = new ClaimedEnqueueReaper("job", () => { runs += 1; Enqueued(1, 0) }, 1.hour, 0.minutes, windows, clock,
      threadPools = tools.ThreadPools.Unmanaged, ready = () => ready)
    reaper.tickIfClaimed() shouldBe None
    ready = true
    reaper.tickIfClaimed() shouldBe Some(TickRun.Completed)
    runs shouldBe 1
  }
}
