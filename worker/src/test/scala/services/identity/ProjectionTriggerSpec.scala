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
    var settles   = true
    val trigger   = new ProjectionTrigger(() => { runs += 1; settles }, MovieChangeStream.Debounce(30.seconds, 2.minutes), scheduler, clock)
    def after(seconds: Long): Unit = scheduler.advance(Duration.ofSeconds(seconds))
    def afterMillis(millis: Long): Unit = scheduler.advance(Duration.ofMillis(millis))
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

  it should "run an answer's short window within it, however a scrape's long one keeps asking beside it" in {
    val w = new World
    w.trigger.request(); w.after(10)
    w.trigger.request(ProjectionTrigger.Answer); w.trigger.request()   // a scrape asking again must not push the answer back
    w.after(1)
    w.runs shouldBe 1
    (1 to 10).foreach { _ => w.trigger.request(ProjectionTrigger.Answer); w.afterMillis(500) }   // a burst of answers, never a second quiet
    w.runs shouldBe 2                       // at the 5 s cap after the burst's first
    w.after(60)
    w.runs shouldBe 2                       // nothing asked since
  }

  it should "not run until asked" in {
    val w = new World
    w.after(3600)
    w.runs shouldBe 0
  }

  it should "run again after a doubling backoff while a run does not settle, and from the start once one does" in {
    // There is no period to come back on: a refused or failed projection, or one whose film still lacks its TMDB details,
    // is tried again by the trigger itself.
    val w = new World
    w.settles = false
    w.trigger.request(); w.after(30)
    w.runs shouldBe 1
    w.after(59); w.runs shouldBe 1
    w.after(1);  w.runs shouldBe 2           // 1 min
    w.after(120); w.runs shouldBe 3          // 2 min
    w.settles = true
    w.after(240); w.runs shouldBe 4          // 4 min, and settled: no more
    w.after(3600); w.runs shouldBe 4
    w.settles = false
    w.trigger.request(); w.after(30); w.runs shouldBe 5
    w.after(60); w.runs shouldBe 6           // the backoff started over
  }

  it should "leave a run already due sooner when asked to retry one made elsewhere" in {
    val w = new World
    w.trigger.request()                      // due in 30 s
    w.trigger.retry()                        // 1 min: later, so the run due stands
    w.after(30); w.runs shouldBe 1
    w.after(3600); w.runs shouldBe 1
    w.trigger.retry()                        // the hourly reconcile did not settle: 1 min (the settled run reset it)
    w.after(59); w.runs shouldBe 1
    w.after(1); w.runs shouldBe 2
  }
}
