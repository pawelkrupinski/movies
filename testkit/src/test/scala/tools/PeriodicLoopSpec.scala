package tools

import java.time.{Duration, Instant}
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Logger

class PeriodicLoopSpec extends AnyFlatSpec with Matchers {

  private val logger = Logger(classOf[PeriodicLoopSpec])

  private final class Fixture {
    val clock     = new MutableClock(Instant.parse("2026-09-26T10:00:00Z"))
    val scheduler = new ManualScheduler(clock)
    val ticks     = new RecordingTickRecorder
    val runs      = new AtomicInteger
  }

  "A periodic loop" should "run first after its initial delay, then once per interval" in {
    val f    = new Fixture
    val loop = PeriodicLoop.rereading("loop", () => { f.runs.incrementAndGet(); TickRun.Completed }, 20.millis, 50.millis, f.ticks, logger, f.scheduler)
    loop.start()
    f.scheduler.advance(Duration.ofMillis(49)); f.runs.get() shouldBe 0
    f.scheduler.advance(Duration.ofMillis(1));  f.runs.get() shouldBe 1
    f.scheduler.advance(Duration.ofMillis(40)); f.runs.get() shouldBe 3
  }

  it should "report each completed run to the tick recorder under its name and interval" in {
    val f = new Fixture
    PeriodicLoop.rereading("recorded", () => TickRun.Completed, 20.millis, 0.millis, f.ticks, logger, f.scheduler).start()
    f.scheduler.advance(Duration.ofMillis(20))
    f.ticks.completedRuns shouldBe Seq("recorded" -> 20.millis, "recorded" -> 20.millis)
  }

  it should "not count an incomplete run as completed" in {
    val f = new Fixture
    PeriodicLoop.rereading("incomplete", () => TickRun.Incomplete, 20.millis, 0.millis, f.ticks, logger, f.scheduler).start()
    f.scheduler.advance(Duration.ofMillis(20))
    f.ticks.completedRuns shouldBe empty
  }

  it should "log a failing run and keep its schedule, without recording it" in {
    val f = new Fixture
    val loop = PeriodicLoop.rereading("failing", () => { f.runs.incrementAndGet(); throw new IllegalStateException("store unreadable") },
      20.millis, 0.millis, f.ticks, logger, f.scheduler)
    val logged = LogCapture.capture(classOf[PeriodicLoopSpec].getName) {
      loop.start()
      f.scheduler.advance(Duration.ofMillis(20))
    }
    f.runs.get() shouldBe 2
    f.ticks.completedRuns shouldBe empty
    logged.exists(e => e.getFormattedMessage.contains("failing tick failed") && e.getFormattedMessage.contains("store unreadable")) shouldBe true
  }

  "A fixed-rate loop" should "arm the scheduler's fixed rate once, with its initial delay and interval, and never reschedule" in {
    val scheduler = new RecordingScheduler
    PeriodicLoop.fixedRate("rate", () => TickRun.Completed, 30.seconds, 5.seconds, TickRecorder.Unrecorded, logger, scheduler).start()
    scheduler.fixedRates.toList shouldBe List(5000L -> 30000L)
    scheduler.oneShots shouldBe empty
  }

  it should "log a failing run, keep its rate and record only the runs that completed" in {
    val f = new Fixture
    val results = Iterator[() => TickRun](() => throw new IllegalStateException("boom"), () => TickRun.Completed)
    PeriodicLoop.fixedRate("rate-failing", () => results.next()(), 20.millis, 0.millis, f.ticks, logger, f.scheduler).start()
    val logged = LogCapture.capture(classOf[PeriodicLoopSpec].getName) { f.scheduler.advance(Duration.ofMillis(20)) }
    f.ticks.completedRuns shouldBe Seq("rate-failing" -> 20.millis)
    logged.exists(_.getFormattedMessage.contains("rate-failing tick failed")) shouldBe true
  }

  "A rereading loop" should "schedule one run at a time, never a fixed rate" in {
    val scheduler = new RecordingScheduler
    PeriodicLoop.rereading("reread", () => TickRun.Completed, 30.seconds, 5.seconds, TickRecorder.Unrecorded, logger, scheduler).start()
    scheduler.oneShots.toList shouldBe List(5000L)
    scheduler.fixedRates shouldBe empty
  }
}
