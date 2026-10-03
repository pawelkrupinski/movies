package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.schedule.InMemoryScheduledRunStore
import tools.{LogCapture, ManualScheduler, MutableClock}

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
      20.millis, 0.millis, new InMemoryScheduledRunStore, clock, _ => scheduler)
    val logged = LogCapture.capture(classOf[ClaimedPeriodicTask].getName) {
      task.start()
      scheduler.advance(Duration.ofMillis(20)) // the first run, then one interval on: the second
      task.stop()
    }
    runs.get() shouldBe 2
    logged.map(_.getFormattedMessage).filter(_.contains("failing-job tick failed")) should not be empty
    logged.exists(e => e.getFormattedMessage.contains("store unreadable")) shouldBe true
  }
}
