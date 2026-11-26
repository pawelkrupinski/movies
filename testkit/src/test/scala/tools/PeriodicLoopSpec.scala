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
    val loop = new PeriodicLoop("loop", () => { f.runs.incrementAndGet(); TickRun.Completed }, 20.millis, 50.millis, f.ticks, logger, f.scheduler)
    loop.start()
    f.scheduler.advance(Duration.ofMillis(49)); f.runs.get() shouldBe 0
    f.scheduler.advance(Duration.ofMillis(1));  f.runs.get() shouldBe 1
    f.scheduler.advance(Duration.ofMillis(40)); f.runs.get() shouldBe 3
  }

  it should "report each completed run to the tick recorder under its name and interval" in {
    val f = new Fixture
    new PeriodicLoop("recorded", () => TickRun.Completed, 20.millis, 0.millis, f.ticks, logger, f.scheduler).start()
    f.scheduler.advance(Duration.ofMillis(20))
    f.ticks.completedRuns shouldBe Seq("recorded" -> 20.millis, "recorded" -> 20.millis)
  }

  it should "not count an incomplete run as completed" in {
    val f = new Fixture
    new PeriodicLoop("incomplete", () => TickRun.Incomplete, 20.millis, 0.millis, f.ticks, logger, f.scheduler).start()
    f.scheduler.advance(Duration.ofMillis(20))
    f.ticks.completedRuns shouldBe empty
  }

  it should "log a failing run and keep its schedule, without recording it" in {
    val f = new Fixture
    val loop = new PeriodicLoop("failing", () => { f.runs.incrementAndGet(); throw new IllegalStateException("store unreadable") },
      20.millis, 0.millis, f.ticks, logger, f.scheduler)
    val logged = LogCapture.capture(classOf[PeriodicLoopSpec].getName) {
      loop.start()
      f.scheduler.advance(Duration.ofMillis(20))
    }
    f.runs.get() shouldBe 2
    f.ticks.completedRuns shouldBe empty
    logged.exists(e => e.getFormattedMessage.contains("failing tick failed") && e.getFormattedMessage.contains("store unreadable")) shouldBe true
  }

  "PeriodicLoop.completing" should "count every run that returns as completed" in {
    val f = new Fixture
    PeriodicLoop.completing("unit-run", () => f.runs.incrementAndGet(), 20.millis, 0.millis, f.ticks, logger, f.scheduler).start()
    f.scheduler.advance(Duration.ofMillis(20))
    f.ticks.completedRuns.size shouldBe 2
  }
}
