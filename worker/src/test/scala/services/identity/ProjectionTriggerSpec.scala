package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.MovieChangeStream
import tools.{ManualScheduler, MutableClock}

import java.time.{Duration, Instant}
import scala.concurrent.duration._

/** [[ProjectionTrigger]] runs a projection once a burst of the model's batches goes quiet, and never later than the cap
 *  after the burst's first — so a scrape's change reaches the films within the cap, a burst in one projection. */
class ProjectionTriggerSpec extends AnyFlatSpec with Matchers {

  private final class World {
    val clock     = new MutableClock(Instant.parse("2026-10-04T18:00:00Z"))
    val scheduler = new ManualScheduler(clock)
    var runs      = 0
    val trigger   = new ProjectionTrigger(() => runs += 1, MovieChangeStream.Debounce(30.seconds, 2.minutes), scheduler, clock)
    def after(seconds: Long): Unit = scheduler.advance(Duration.ofSeconds(seconds))
  }

  "a projection trigger" should "run once a burst goes quiet, the burst in one run" in {
    val w = new World
    w.trigger.request(); w.after(10); w.trigger.request(); w.after(10); w.trigger.request()
    w.after(29)
    w.runs shouldBe 0                       // each request pushed it 30 s on
    w.after(1)
    w.runs shouldBe 1
    w.after(600)
    w.runs shouldBe 1                       // nothing asked since
  }

  it should "run at the cap after a burst's first request however long the burst" in {
    val w = new World
    (1 to 12).foreach { _ => w.trigger.request(); w.after(20) }   // never 30 s quiet, 240 s long
    w.runs shouldBe 2                       // at 120 s, and 120 s after the next burst's first, at 240 s
  }

  it should "not run until asked" in {
    val w = new World
    w.after(3600)
    w.runs shouldBe 0
  }
}
