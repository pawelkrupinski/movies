package services.tasks

import models.{CinemaMovie, KinoApollo, Movie, MovieRecord, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import services.schedule.{InMemoryScheduledRunStore, NeverClaimScheduledRunStore}

import java.time.{Instant, LocalDateTime}
import scala.concurrent.duration._

class EnrichmentReaperSpec extends AnyFlatSpec with Matchers {

  private val t0 = Instant.parse("2026-06-18T00:00:00Z").toEpochMilli

  private def newCache() = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus())

  private def seedRow(cache: CaffeineMovieCache, title: String)(edit: MovieRecord => MovieRecord): Unit = {
    cache.recordCinemaScrape(KinoApollo, Seq(CinemaMovie(
      Movie(title), KinoApollo, None, None, None, Seq.empty, Seq.empty,
      Seq(Showtime(LocalDateTime.of(2026, 6, 8, 18, 0), Some("https://book"))))))
    cache.putIfPresent(cache.keyOf(title, None), edit)
  }

  // ── eligibility ─────────────────────────────────────────────────────────────

  "EnrichmentReaper.tick" should "enqueue a task per source for a fully-resolved row, and dedup on a re-tick" in {
    val cache = newCache(); val queue = new InMemoryTaskQueue
    seedRow(cache, "Resolved")(_.copy(imdbId = Some("tt1"), tmdbId = Some(2))) // eligible for all 4 sources
    val reaper = new EnrichmentReaper(cache, queue, new InMemoryFreshnessStore)
    reaper.tick(t0) shouldBe 4
    reaper.tick(t0) shouldBe 0 // same instant, tasks still waiting → deduped
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 4L
  }

  it should "enqueue only the non-IMDb ratings for a TMDB-resolved row without an imdbId" in {
    val cache = newCache(); val queue = new InMemoryTaskQueue
    seedRow(cache, "TmdbOnly")(_.copy(tmdbId = Some(2)))
    new EnrichmentReaper(cache, queue, new InMemoryFreshnessStore).tick(t0) shouldBe 3
  }

  it should "enqueue nothing for an unresolved row" in {
    val cache = newCache(); val queue = new InMemoryTaskQueue
    seedRow(cache, "Bare")(identity)
    new EnrichmentReaper(cache, queue, new InMemoryFreshnessStore).tick(t0) shouldBe 0
  }

  // ── due / freshness boundary ─────────────────────────────────────────────────

  it should "treat a never-refreshed eligible row as due immediately" in {
    val cache = newCache(); val queue = new InMemoryTaskQueue
    seedRow(cache, "Fresh")(_.copy(imdbId = Some("tt1"))) // IMDb-only eligible
    new EnrichmentReaper(cache, queue, new InMemoryFreshnessStore).tick(t0) shouldBe 1
  }

  it should "not re-enqueue a row refreshed inside the current window, but does once a period boundary passes" in {
    val cache = newCache(); val queue = new InMemoryTaskQueue; val fresh = new InMemoryFreshnessStore
    seedRow(cache, "Stamped")(_.copy(imdbId = Some("tt1")))
    fresh.markFresh(RatingTasks.dedupKey(FreshnessKind.ImdbRating, cache.keyOf("Stamped", None)),
      FreshnessKind.ImdbRating, Instant.ofEpochMilli(t0))
    val reaper = new EnrichmentReaper(cache, queue, fresh, dueWindow = new DueWindow(4.hours))
    reaper.tick(t0) shouldBe 0                           // same window as the stamp
    reaper.tick(t0 + 2 * 4.hours.toMillis) shouldBe 1    // two periods later → crossed a boundary
  }

  // ── the spread property ──────────────────────────────────────────────────────

  it should "spread a synchronized corpus about-evenly across the period rather than bursting" in {
    val cache = newCache(); val queue = new InMemoryTaskQueue; val fresh = new InMemoryFreshnessStore
    val period = 4.hours; val delta = 5.minutes
    val n = 120
    // n IMDb-only rows, all stamped fresh at the SAME instant — a synchronized
    // "birthday" that the old burst walk would re-enqueue all at once one period on.
    (0 until n).foreach { i =>
      val title = f"Film$i%03d"
      seedRow(cache, title)(_.copy(imdbId = Some(s"tt$i")))
      fresh.markFresh(RatingTasks.dedupKey(FreshnessKind.ImdbRating, cache.keyOf(title, None)),
        FreshnessKind.ImdbRating, Instant.ofEpochMilli(t0))
    }
    val reaper = new EnrichmentReaper(cache, queue, fresh, dueWindow = new DueWindow(period))
    val ticks  = (period.toMillis / delta.toMillis).toInt // 48 ticks across one period
    val perTick = (1 to ticks).map(k => reaper.tick(t0 + k * delta.toMillis))

    perTick.sum shouldBe n                       // every row refreshed exactly once over the period
    perTick.max should be <= 20                  // no burst (a single-shot walk would put all 120 in one tick)
    perTick.count(_ > 0) should be >= 24         // genuinely spread across at least half the ticks
  }

  // ── recovery-burst cap ───────────────────────────────────────────────────────

  it should "cap enqueues per tick so a cold corpus drains over several ticks instead of all at once" in {
    val cache = newCache(); val queue = new InMemoryTaskQueue
    (0 until 50).foreach(i => seedRow(cache, f"Cold$i%03d")(_.copy(imdbId = Some(s"tt$i")))) // all never-refreshed → due
    val reaper = new EnrichmentReaper(cache, queue, new InMemoryFreshnessStore, maxEnqueuePerTick = 10)
    reaper.tick(t0) shouldBe 10                   // first batch only
    reaper.tick(t0) shouldBe 10                   // next batch (first 10 still waiting → deduped)
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 20L
  }

  // ── cluster occurrence claim ─────────────────────────────────────────────────

  "EnrichmentReaper.tickIfClaimed" should "not enqueue when another machine has claimed the tick window" in {
    val cache = newCache(); val queue = new InMemoryTaskQueue
    seedRow(cache, "Resolved")(_.copy(imdbId = Some("tt1"), tmdbId = Some(2)))
    new EnrichmentReaper(cache, queue, new InMemoryFreshnessStore, runStore = NeverClaimScheduledRunStore)
      .tickIfClaimed() shouldBe 0
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 0L
  }

  it should "tick when it wins the claim" in {
    val cache = newCache(); val queue = new InMemoryTaskQueue
    seedRow(cache, "Resolved")(_.copy(imdbId = Some("tt1"), tmdbId = Some(2)))
    new EnrichmentReaper(cache, queue, new InMemoryFreshnessStore, runStore = new InMemoryScheduledRunStore)
      .tickIfClaimed() shouldBe 4
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 4L
  }
}
