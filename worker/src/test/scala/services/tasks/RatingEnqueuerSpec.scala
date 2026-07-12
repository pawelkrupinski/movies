package services.tasks

import models.{Country, MovieRecord}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.CacheKey

import java.time.Instant
import scala.concurrent.duration._

class RatingEnqueuerSpec extends AnyFlatSpec with Matchers {

  private val now = Instant.parse("2026-07-06T00:00:00Z")

  private def enqueuer(queue: TaskQueue, country: Country = Country.default) =
    new RatingEnqueuer(queue, new InMemoryFreshnessStore, new DueWindow(4.hours), country)

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

  it should "force-re-enqueue every source even when its freshness stamp is still fresh (a re-resolve stripped the scores)" in {
    val queue     = new InMemoryTaskQueue
    val freshness = new InMemoryFreshnessStore
    val enq       = new RatingEnqueuer(queue, freshness, new DueWindow(4.hours))
    val key       = CacheKey("Film", None)
    val row       = MovieRecord(imdbId = Some("tt1"), tmdbId = Some(2))
    // Stamp all four sources fresh — the cadence now judges each "recently checked".
    Seq(FreshnessKind.ImdbRating, FreshnessKind.FilmwebRating, FreshnessKind.RtRating, FreshnessKind.McRating)
      .foreach(k => freshness.markFresh(RatingTasks.dedupKey(k, key, row.tmdbId), k, now))

    // The normal, cadence-gated enqueue does nothing — every source reads fresh.
    enq.enqueueDueFor(key, row, now) shouldBe 0
    // Forcing drops the stamps and re-enqueues all four regardless.
    enq.enqueueDueFor(key, row, now, force = true) shouldBe 4
    Seq(TaskType.ImdbRating, TaskType.FilmwebRating, TaskType.RtRating, TaskType.McRating)
      .foreach(queue.waitingCount(_) shouldBe 1)
  }

  it should "NOT enqueue Filmweb in a non-Filmweb country (UK) — its handler is unwired there, so the task would re-release forever" in {
    val queue = new InMemoryTaskQueue
    val row   = MovieRecord(imdbId = Some("tt1"), tmdbId = Some(2))
    // Fully resolved: eligible for all four sources, cap unbounded — yet UK drops Filmweb.
    enqueuer(queue, Country.UnitedKingdom).enqueueDueFor(CacheKey("Film", None), row, now) shouldBe 3
    queue.waitingCount(TaskType.FilmwebRating) shouldBe 0
    Seq(TaskType.ImdbRating, TaskType.RtRating, TaskType.McRating)
      .foreach(queue.waitingCount(_) shouldBe 1)
  }
}
