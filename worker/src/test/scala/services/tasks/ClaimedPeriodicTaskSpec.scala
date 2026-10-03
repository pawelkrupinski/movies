package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.schedule.InMemoryScheduledRunStore
import tools.{LogCapture, MutableClock}

import java.time.{Duration, Instant}
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*

class ClaimedPeriodicTaskSpec extends AnyFlatSpec with Matchers {

  // Every scheduler wrapped its tick in a bare `Try`, so a task failing on every run kept being
  // scheduled and said nothing — indistinguishable in the logs from one with nothing to do.
  "A periodic task whose run throws" should "log the failure and keep its schedule" in {
    val runs  = new AtomicInteger
    val clock = new MutableClock(Instant.parse("2026-09-26T10:00:00Z"))
    val task = new ClaimedPeriodicTask("failing-job", () => { runs.incrementAndGet(); throw new IllegalStateException("store unreadable") },
      20.millis, 0.millis, new InMemoryScheduledRunStore, clock)
    val logged = LogCapture.capture(classOf[ClaimedPeriodicTask].getName) {
      task.start()
      val deadline = System.nanoTime() + 5.seconds.toNanos
      while (runs.get() < 2 && System.nanoTime() < deadline) { clock.advance(Duration.ofMillis(20)); Thread.sleep(10) }
      task.stop()
    }
    runs.get() should be >= 2
    logged.map(_.getFormattedMessage).filter(_.contains("failing-job tick failed")) should not be empty
    logged.exists(e => e.getFormattedMessage.contains("store unreadable")) shouldBe true
  }
}
