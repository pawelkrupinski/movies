package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.schedule.InMemoryScheduledRunStore
import tools.{LogCapture, ManualScheduler, MutableClock, TickRecorder}

import java.time.{Duration, Instant}
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*

class ClaimedPeriodicTaskSpec extends AnyFlatSpec with Matchers {

  // Every scheduler wrapped its tick in a bare `Try`, so a task failing on every run kept being
  // scheduled and said nothing — indistinguishable in the logs from one with nothing to do.
  "A periodic task whose run throws" should "log the failure and keep its schedule" in {
    val runs  = new AtomicInteger
    val clock = new MutableClock(Instant.parse("2026-09-26T10:00:00Z"))
    val scheduler = new ManualScheduler(clock)
    val task = new ClaimedPeriodicTask("failing-job", () => { runs.incrementAndGet(); throw new IllegalStateException("store unreadable") },
      20.millis, 0.millis, new InMemoryScheduledRunStore, clock, TickRecorder.Unrecorded, _ => scheduler)
    val logged = LogCapture.capture(classOf[ClaimedPeriodicTask].getName) {
      task.start()
      scheduler.advance(Duration.ofMillis(20)) // the first run, then one interval on: the second
      task.stop()
    }
    runs.get() shouldBe 2
    logged.map(_.getFormattedMessage).filter(_.contains("failing-job tick failed")) should not be empty
    logged.exists(e => e.getFormattedMessage.contains("store unreadable")) shouldBe true
  }

  // The next run's delay was read outside the logged tick: an interval that threw ended the
  // schedule for good, silently. It keeps the last delay and says why.
  "A periodic task whose interval cannot be read" should "keep its schedule on the delay it had, at least the minimum, and log why" in {
    val runs      = new AtomicInteger
    val clock     = new MutableClock(Instant.parse("2026-09-26T10:00:00Z"))
    val scheduler = new ManualScheduler(clock)
    @volatile var broken = false
    // Unreadable once, right after the first run: the read that picks the next run's delay.
    def interval: FiniteDuration = if (broken) { broken = false; throw new IllegalStateException("config unreadable") } else 20.millis
    val task = new ClaimedPeriodicTask("unread-interval", () => { if (runs.incrementAndGet() == 1) broken = true },
      interval, 20.millis, new InMemoryScheduledRunStore, clock, TickRecorder.Unrecorded, _ => scheduler)
    val logged = LogCapture.capture(classOf[ClaimedPeriodicTask].getName) {
      task.start()
      scheduler.advance(Duration.ofMillis(20 + ClaimedPeriodicTask.MinimumFallbackDelay.toMillis))
      task.stop()
    }
    runs.get() should be >= 2
    logged.exists(_.getFormattedMessage.contains("config unreadable")) shouldBe true
  }

  // The fallback was the last delay, which on the first tick is the initial delay: a zero one re-ran
  // the tick back to back, forever, while the interval stayed unreadable. It is floored.
  it should "not re-run back to back when its initial delay was zero" in {
    val clock     = new MutableClock(Instant.parse("2026-09-26T10:00:00Z"))
    val scheduler = new ManualScheduler(clock)
    val reads     = new AtomicInteger
    @volatile var broken = false
    // Readable again after 100 broken reads, so a spinning schedule ends and the count shows it.
    def interval: FiniteDuration =
      if (!broken) 1.hour
      else if (reads.incrementAndGet() >= 100) 1.hour
      else throw new IllegalStateException("config unreadable")
    val task = new ClaimedPeriodicTask("zero-initial-delay", () => (), interval, 0.millis, new InMemoryScheduledRunStore, clock, TickRecorder.Unrecorded, _ => scheduler)
    LogCapture.capture(classOf[ClaimedPeriodicTask].getName) {
      task.start()
      broken = true
      scheduler.advance(Duration.ofMillis(ClaimedPeriodicTask.MinimumFallbackDelay.toMillis - 1))
      task.stop()
    }
    reads.get() shouldBe 2 // the first tick's claim and its next delay, then nothing for a minute
  }

  // The pulse `ScheduledTickStale` reads: armed with its first run's due time, stamped after every
  // run that returned, never after one that threw -- a task failing every run has stopped running.
  "A periodic task" should "tell its tick recorder when it is armed and after each run that returned" in {
    val clock     = new MutableClock(Instant.parse("2026-09-26T10:00:00Z"))
    val scheduler = new ManualScheduler(clock)
    val events    = new java.util.concurrent.ConcurrentLinkedQueue[String]
    val ticks = new TickRecorder {
      def started(tick: String, interval: FiniteDuration, firstRunIn: FiniteDuration): Unit =
        events.add(s"started $tick every ${interval.toMillis}ms, first in ${firstRunIn.toMillis}ms")
      def completed(tick: String, interval: FiniteDuration): Unit = events.add(s"completed $tick every ${interval.toMillis}ms")
    }
    val runs = new AtomicInteger
    val task = new ClaimedPeriodicTask("recorded-job", () => { if (runs.incrementAndGet() == 2) throw new IllegalStateException("boom") },
      20.millis, 5.millis, new InMemoryScheduledRunStore, clock, ticks, _ => scheduler)
    LogCapture.capture(classOf[ClaimedPeriodicTask].getName) {
      task.start()
      scheduler.advance(Duration.ofMillis(5 + 20 + 20)) // three runs: fine, throws, fine
      task.stop()
    }
    runs.get() shouldBe 3
    import scala.jdk.CollectionConverters.*
    events.asScala.toList shouldBe List(
      "started recorded-job every 20ms, first in 5ms", "completed recorded-job every 20ms", "completed recorded-job every 20ms")
  }
}
