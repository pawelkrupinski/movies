package services.tasks

import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.freshness.InMemoryFreshnessStore
import services.movies.CacheKey

import java.time.Instant
import scala.concurrent.duration._

class RatingEnqueuerSpec extends AnyFlatSpec with Matchers {

  private val now = Instant.parse("2026-07-06T00:00:00Z")

  private def enqueuer(queue: TaskQueue) =
    new RatingEnqueuer(queue, new InMemoryFreshnessStore, new DueWindow(4.hours))

  "RatingEnqueuer" should "enqueue Filmweb RIGHT AFTER IMDb — before RT/MC — under a per-tick cap" in {
    // Filmweb is a full enrichment peer of IMDb now (RatingSources order), so
    // when the reaper's cap can't fit all four eligible sources, Filmweb takes
    // the slot ahead of RT/MC. A fully-resolved row is eligible for all four;
    // cap the enqueue at 2 and only IMDb + Filmweb should land.
    val queue = new InMemoryTaskQueue
    val row   = MovieRecord(imdbId = Some("tt1"), tmdbId = Some(2))

    enqueuer(queue).enqueueDueFor(CacheKey("Film", None), row, now, limit = 2) shouldBe 2

    queue.waitingCount(TaskType.ImdbRating)    shouldBe 1
    queue.waitingCount(TaskType.FilmwebRating) shouldBe 1
    queue.waitingCount(TaskType.RtRating)      shouldBe 0
    queue.waitingCount(TaskType.McRating)      shouldBe 0
  }

  it should "enqueue all four eligible sources when the cap allows" in {
    val queue = new InMemoryTaskQueue
    val row   = MovieRecord(imdbId = Some("tt1"), tmdbId = Some(2))
    enqueuer(queue).enqueueDueFor(CacheKey("Film", None), row, now) shouldBe 4
    Seq(TaskType.ImdbRating, TaskType.FilmwebRating, TaskType.RtRating, TaskType.McRating)
      .foreach(queue.waitingCount(_) shouldBe 1)
  }

  it should "never enqueue FilmwebRating for a filmweb-disabled country (no handler exists there)" in {
    // The regression: a UK/DE (filmwebEnabled=false) wiring has no FilmwebRating
    // handler, so enqueuing that TaskType produces a task that retries forever
    // ("no handler for FilmwebRating") and starves the scrape queue. The wiring
    // hands this enqueuer the filmweb-filtered source list; a fully-resolved row
    // (eligible for all four) must then enqueue only THREE — never Filmweb.
    val queue = new InMemoryTaskQueue
    val row   = MovieRecord(imdbId = Some("tt1"), tmdbId = Some(2))
    val filmwebDisabled = new RatingEnqueuer(
      queue, new InMemoryFreshnessStore, new DueWindow(4.hours),
      sources = RatingSources.forCountry(filmwebEnabled = false))

    filmwebDisabled.enqueueDueFor(CacheKey("Film", None), row, now) shouldBe 3
    queue.waitingCount(TaskType.FilmwebRating) shouldBe 0
    Seq(TaskType.ImdbRating, TaskType.RtRating, TaskType.McRating)
      .foreach(queue.waitingCount(_) shouldBe 1)
  }
}
