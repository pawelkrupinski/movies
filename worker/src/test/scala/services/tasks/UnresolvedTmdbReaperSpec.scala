package services.tasks

import models.{CinemaMovie, KinoApollo, Movie, MovieRecord, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.movies.{CacheKey, CaffeineMovieCache, InMemoryMovieRepository}
import services.schedule.{InMemoryScheduledRunStore, NeverClaimScheduledRunStore}

import java.time.{Instant, LocalDateTime}
import scala.collection.mutable
import scala.concurrent.duration._

class UnresolvedTmdbReaperSpec extends AnyFlatSpec with Matchers {

  private val t0 = Instant.parse("2026-06-18T00:00:00Z").toEpochMilli

  private def newCache() = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus())

  private def seedRow(cache: CaffeineMovieCache, title: String)(edit: MovieRecord => MovieRecord): Unit = {
    cache.recordCinemaScrape(KinoApollo, Seq(CinemaMovie(
      Movie(title), KinoApollo, None, None, None, Seq.empty, Seq.empty,
      Seq(Showtime(LocalDateTime.of(2026, 6, 8, 18, 0), Some("https://book"))))))
    cache.putIfPresent(cache.keyOf(title, None), edit)
  }

  /** A recording retry seam — the reaper owns the schedule/gate; the per-row
   *  re-try (clear negative + dispatch) is `MovieService.retryResolve` in prod. */
  private def recorder(): (mutable.Buffer[CacheKey], CacheKey => Unit) = {
    val seen = mutable.Buffer.empty[CacheKey]
    (seen, key => { seen += key; () })
  }

  /** Drive a full period of contiguous 5-min ticks (the default tick interval),
   *  returning the per-tick re-try counts. */
  private def runOnePeriod(reaper: UnresolvedTmdbReaper, period: FiniteDuration = 24.hours): Seq[Int] = {
    val delta = UnresolvedTmdbReaper.DefaultTickInterval
    val ticks = (period.toMillis / delta.toMillis).toInt
    (1 to ticks).map(k => reaper.tick(t0 + k * delta.toMillis))
  }

  // ── eligibility (independent of phase: assert over a whole period) ────────────

  "UnresolvedTmdbReaper" should "re-try an unresolved row exactly once per period" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "Bare")(identity) // tmdbId = None → unresolved
    runOnePeriod(new UnresolvedTmdbReaper(cache, retry)).sum shouldBe 1
    seen.map(_.cleanTitle).distinct should contain only cache.keyOf("Bare", None).cleanTitle
  }

  it should "never re-try a TMDB-resolved row" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "Resolved")(_.copy(tmdbId = Some(42)))
    runOnePeriod(new UnresolvedTmdbReaper(cache, retry)).sum shouldBe 0
    seen shouldBe empty
  }

  it should "never re-try a row still awaiting detail enrichment" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "Pending")(_.copy(detailPending = true)) // unresolved but detail owed
    runOnePeriod(new UnresolvedTmdbReaper(cache, retry)).sum shouldBe 0
    seen shouldBe empty
  }

  // ── the spread property ──────────────────────────────────────────────────────

  it should "spread a synchronized unresolved backlog across the period rather than bursting" in {
    val cache = newCache(); val (_, retry) = recorder()
    val n = 150
    (0 until n).foreach(i => seedRow(cache, f"Stuck$i%03d")(identity)) // all unresolved, all "born" together
    val reaper = new UnresolvedTmdbReaper(cache, retry)
    val perTick = runOnePeriod(reaper)

    perTick.sum shouldBe n                  // every unresolved row re-tried exactly once over the period
    perTick.max should be <= 20             // no burst (the old daily sweep put all 150 in one tick)
    perTick.count(_ > 0) should be >= 100   // genuinely spread across most ticks
  }

  // ── recovery-burst cap ───────────────────────────────────────────────────────

  it should "cap re-tries per tick so a bunched backlog drains over several ticks" in {
    val cache = newCache(); val (_, retry) = recorder()
    (0 until 50).foreach(i => seedRow(cache, f"Stuck$i%03d")(identity))
    // period == tickInterval → every unresolved row is due on every tick, so the
    // cap is what bounds the batch (the leftover stays due next tick).
    val reaper = new UnresolvedTmdbReaper(cache, retry,
      dueWindow = new DueWindow(UnresolvedTmdbReaper.DefaultTickInterval), maxEnqueuePerTick = 10)
    reaper.tick(t0) shouldBe 10
    reaper.tick(t0 + UnresolvedTmdbReaper.DefaultTickInterval.toMillis) shouldBe 10
  }

  it should "back off to throttledMaxEnqueuePerTick while the worker is CPU-credit throttled" in {
    val cache = newCache(); val (_, retry) = recorder()
    (0 until 50).foreach(i => seedRow(cache, f"Stuck$i%03d")(identity))
    val throttled = new ScrapeThrottleSignal { def isThrottled = true; def slowScrapeMillis = 30000L }
    val reaper = new UnresolvedTmdbReaper(cache, retry,
      dueWindow = new DueWindow(UnresolvedTmdbReaper.DefaultTickInterval),
      maxEnqueuePerTick = 100, throttledMaxEnqueuePerTick = 5, throttle = throttled)
    reaper.tick(t0) shouldBe 5 // throttled → trickle (vs up to the healthy cap)
  }

  // ── cluster occurrence claim ─────────────────────────────────────────────────

  "UnresolvedTmdbReaper.tickIfClaimed" should "not re-try when another machine claimed the window" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "Bare")(identity)
    new UnresolvedTmdbReaper(cache, retry,
      dueWindow = new DueWindow(UnresolvedTmdbReaper.DefaultTickInterval),
      runStore = NeverClaimScheduledRunStore).tickIfClaimed() shouldBe 0
    seen shouldBe empty
  }

  it should "re-try when it wins the claim" in {
    val cache = newCache(); val (_, retry) = recorder()
    seedRow(cache, "Bare")(identity)
    new UnresolvedTmdbReaper(cache, retry,
      dueWindow = new DueWindow(UnresolvedTmdbReaper.DefaultTickInterval),
      runStore = new InMemoryScheduledRunStore).tickIfClaimed() shouldBe 1
  }
}
