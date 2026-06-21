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

  it should "back off to throttledMaxEnqueuePerTick while the worker is CPU-credit throttled" in {
    // Ratings are the dominant non-scrape load; backing them off (not just scrapes)
    // is what lets the pool idle + rebuild credit. A fully-resolved row is eligible
    // for all 4 sources, but a throttled reaper enqueues only the trickle.
    val cache = newCache(); val queue = new InMemoryTaskQueue
    seedRow(cache, "Resolved")(_.copy(imdbId = Some("tt1"), tmdbId = Some(2)))
    val throttled = new ScrapeThrottleSignal { def isThrottled = true; def ewmaMillis = 30000L }
    val reaper = new EnrichmentReaper(cache, queue, new InMemoryFreshnessStore,
      throttledMaxEnqueuePerTick = 1, throttle = throttled)
    reaper.tick(t0) shouldBe 1 // throttled → 1, not all 4 eligible rating tasks
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

  it should "keep a row's rating freshness across a TITLE re-key (same tmdbId) and not re-enqueue" in {
    val cache = newCache(); val queue = new InMemoryTaskQueue; val fresh = new InMemoryFreshnessStore
    seedRow(cache, "Tangled")(_.copy(imdbId = Some("tt0398286"), tmdbId = Some(38757)))
    val reaper = new EnrichmentReaper(cache, queue, fresh)
    reaper.tick(t0) shouldBe 4
    // Simulate the four handlers refreshing: stamp each enqueued task's key fresh.
    Iterator.continually(queue.claim("w", 1.minute, Instant.ofEpochMilli(t0)))
      .takeWhile(_.isDefined).flatten.foreach { task =>
        fresh.markFresh(task.dedupKey, FreshnessKind.ImdbRating, Instant.ofEpochMilli(t0))
        queue.complete(task.id, "w")
      }
    // The SAME film is re-listed under its Polish title — a cross-language fold /
    // title-rule merge: same tmdbId, NEW cache key. Rating freshness is keyed on the
    // stable tmdbId, so the re-keyed row stays fresh and is NOT re-enqueued. (Keyed
    // on title|year, every such re-key orphaned the stamp and re-queued all four
    // sources — the corpus-wide surge that pinned the worker after the merge waves.)
    cache.invalidate(cache.keyOf("Tangled", None))
    seedRow(cache, "Zaplątani")(_.copy(imdbId = Some("tt0398286"), tmdbId = Some(38757)))
    reaper.tick(t0) shouldBe 0
  }

  it should "honour a legacy title-keyed stamp so switching to tmdbId keys doesn't re-queue the corpus" in {
    val cache = newCache(); val queue = new InMemoryTaskQueue; val fresh = new InMemoryFreshnessStore
    seedRow(cache, "Resolved")(_.copy(imdbId = Some("tt1"), tmdbId = Some(2)))
    // Stamps written by the OLD title-keyed code, still fresh in this window. The
    // reaper must treat the row as fresh under the new tmdbId key via the fallback.
    Seq(FreshnessKind.ImdbRating, FreshnessKind.RtRating, FreshnessKind.McRating, FreshnessKind.FilmwebRating)
      .foreach(k => fresh.markFresh(RatingTasks.dedupKey(k, cache.keyOf("Resolved", None)), k, Instant.ofEpochMilli(t0)))
    new EnrichmentReaper(cache, queue, fresh).tick(t0) shouldBe 0
  }

  // ── the spread property ──────────────────────────────────────────────────────

  /** Drive `n` IMDb-only rows — all stamped fresh at the SAME instant (a
   *  synchronized "birthday" the old burst walk re-enqueued all at once one
   *  period on) — through one full `period` of ticks spaced `delta` apart, and
   *  return the per-tick enqueue counts. The reaper's phase spread should smear
   *  them across the ticks rather than bunch them. */
  private def perTickOverPeriod(n: Int, delta: FiniteDuration, period: FiniteDuration = 4.hours): Seq[Int] = {
    val cache = newCache(); val queue = new InMemoryTaskQueue; val fresh = new InMemoryFreshnessStore
    (0 until n).foreach { i =>
      val title = f"Film$i%03d"
      seedRow(cache, title)(_.copy(imdbId = Some(s"tt$i")))
      fresh.markFresh(RatingTasks.dedupKey(FreshnessKind.ImdbRating, cache.keyOf(title, None)),
        FreshnessKind.ImdbRating, Instant.ofEpochMilli(t0))
    }
    val reaper = new EnrichmentReaper(cache, queue, fresh, dueWindow = new DueWindow(period))
    val ticks  = (period.toMillis / delta.toMillis).toInt
    (1 to ticks).map(k => reaper.tick(t0 + k * delta.toMillis))
  }

  it should "spread a synchronized corpus about-evenly across the period rather than bursting" in {
    val perTick = perTickOverPeriod(n = 120, delta = 5.minutes) // 48 ticks across one period
    perTick.sum shouldBe 120                      // every row refreshed exactly once over the period
    perTick.max should be <= 20                   // no burst (a single-shot walk would put all 120 in one tick)
    perTick.count(_ > 0) should be >= 24          // genuinely spread across at least half the ticks
  }

  // The actual smoothing lever for the prod `kinowo_worker_tasks` rating spikes:
  // a tick interval `delta` only catches the rows whose phase boundary fell in the
  // last `delta`, so the per-tick burst scales with `delta`. The production wiring
  // uses `EnrichmentReaper.DefaultTickInterval`, so smear is only as fine as that
  // default. This guards that the default is finer than the old 5-min cadence —
  // a finer default genuinely flattens the worst-case per-tick burst for the same
  // synchronized corpus. (Fails when the default IS 5min: the two runs are
  // identical, so the finer-run max isn't materially below the 5-min max.)
  it should "keep the per-tick burst materially flatter at the default interval than at the old 5-min cadence" in {
    val n = 240
    val coarseMax = perTickOverPeriod(n, delta = 5.minutes).max
    val defaultMax = perTickOverPeriod(n, delta = EnrichmentReaper.DefaultTickInterval).max
    EnrichmentReaper.DefaultTickInterval should be < (5.minutes: FiniteDuration)
    defaultMax.toDouble should be <= (coarseMax / 2.0)
  }

  // ── recovery-burst cap ───────────────────────────────────────────────────────

  it should "read maxEnqueuePerTick live each tick, so an /admin/config cap flip applies mid-flight" in {
    val cache = newCache(); val queue = new InMemoryTaskQueue
    (0 until 10).foreach(i => seedRow(cache, f"Live$i%03d")(_.copy(imdbId = Some(s"tt$i")))) // 10 cold → due
    var cap = 1
    val reaper = new EnrichmentReaper(cache, queue, new InMemoryFreshnessStore, maxEnqueuePerTick = cap)
    reaper.tick(t0) shouldBe 1   // cap = 1
    cap = 5
    reaper.tick(t0) shouldBe 5   // live re-read picks up the new cap (a captured Int would still be 1)
  }

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

  it should "hold its tick (enqueue nothing) while the rating freshness mirror is still hydrating" in {
    // A resolved, never-refreshed row is eligible for all 4 sources → due. But the
    // rating stamps hydrate in the rest phase, so while that's in progress the
    // reaper must NOT read the empty mirror as "all stale" and re-enqueue the
    // corpus — the recurring per-deploy rating spike. It wins the claim yet holds.
    val cache = newCache(); val queue = new InMemoryTaskQueue
    seedRow(cache, "Resolved")(_.copy(imdbId = Some("tt1"), tmdbId = Some(2)))
    val hydrating = new InMemoryFreshnessStore {
      override def whenReady(kind: FreshnessKind): scala.concurrent.Future[Unit] = scala.concurrent.Promise[Unit]().future
    }
    new EnrichmentReaper(cache, queue, hydrating, runStore = new InMemoryScheduledRunStore)
      .tickIfClaimed() shouldBe 0
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 0L
  }
}
