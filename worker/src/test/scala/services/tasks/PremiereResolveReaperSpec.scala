package services.tasks

import models.{CinemaMovie, KinoApollo, Movie, MovieRecord, Showtime, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.movies.{CacheKey, CaffeineMovieCache, InMemoryMovieRepository}
import services.schedule.{InMemoryScheduledRunStore, NeverClaimScheduledRunStore}

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}
import scala.collection.mutable
import scala.concurrent.duration._

class PremiereResolveReaperSpec extends AnyFlatSpec with Matchers {

  private val t0 = Instant.parse("2026-06-18T00:00:00Z").toEpochMilli
  // A premiere 3 days after t0 → inside the default [−1, +7)-day window, so the
  // seeded rows are eligible for the whole driven period (the window edges move
  // <1 day over 24h of ticks, well within the 3-day margin).
  private val inWindowPremiere = LocalDateTime.of(2026, 6, 21, 18, 0)
  // A fixed clock at t0 so `tickIfClaimed` (which reads the clock for both the
  // occurrence key AND the tick time) evaluates the window against t0, not wall time.
  private val clockAtT0 = Clock.fixed(Instant.ofEpochMilli(t0), ZoneOffset.UTC)

  private def newCache() = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus())

  private def cinemaMovie(title: String, premiere: LocalDateTime): CinemaMovie =
    CinemaMovie(Movie(title), KinoApollo, None, None, None, Seq.empty, Seq.empty,
      Seq(Showtime(premiere, Some("https://book"))))

  private def seedRow(cache: CaffeineMovieCache, title: String, premiere: LocalDateTime = inWindowPremiere)
                     (edit: MovieRecord => MovieRecord): Unit = {
    cache.recordCinemaScrape(KinoApollo, Seq(cinemaMovie(title, premiere)))
    cache.putIfPresent(cache.keyOf(title, None), edit)
  }

  /** Seed many unresolved rows in ONE cinema scrape — a cinema scrape REPLACES
   *  that cinema's whole listing, so seeding row-by-row would strip every prior
   *  row's showtimes (leaving them premiere-less and out of window). All share
   *  the in-window premiere. */
  private def seedManyInWindow(cache: CaffeineMovieCache, titles: Seq[String]): Unit =
    cache.recordCinemaScrape(KinoApollo, titles.map(cinemaMovie(_, inWindowPremiere)))

  /** A recording retry seam — the reaper owns the schedule/gate; the per-row
   *  re-try (clear negative + dispatch) is `MovieService.retryResolve` in prod. */
  private def recorder(): (mutable.Buffer[CacheKey], CacheKey => Unit) = {
    val seen = mutable.Buffer.empty[CacheKey]
    (seen, key => { seen += key; () })
  }

  /** Drive a full period of contiguous 5-min ticks (the default tick interval),
   *  returning the per-tick re-try counts. */
  private def runOnePeriod(reaper: PremiereResolveReaper, period: FiniteDuration = 24.hours): Seq[Int] = {
    val delta = PremiereResolveReaper.DefaultTickInterval
    val ticks = (period.toMillis / delta.toMillis).toInt
    (1 to ticks).map(k => reaper.tick(t0 + k * delta.toMillis))
  }

  private def recordWithPremiere(premiere: Option[LocalDateTime]): MovieRecord =
    MovieRecord(data = Map(KinoApollo -> SourceData(showtimes = premiere.map(Showtime(_, None)).toSeq)))

  // ── the premiere-window gate (pure) ──────────────────────────────────────────

  private val gateReaper = new PremiereResolveReaper(newCache(), _ => ())
  private val nowLocal = LocalDateTime.of(2026, 6, 18, 12, 0) // window = [06-17 12:00, 06-25 12:00)

  "inPremiereWindow" should "be true when the first screening is a few days ahead" in {
    gateReaper.inPremiereWindow(recordWithPremiere(Some(LocalDateTime.of(2026, 6, 21, 18, 0))), nowLocal) shouldBe true
  }
  it should "be false when the first screening is well beyond the lead window" in {
    gateReaper.inPremiereWindow(recordWithPremiere(Some(LocalDateTime.of(2026, 7, 18, 18, 0))), nowLocal) shouldBe false
  }
  it should "be false when the first screening is already days in the past (beyond grace)" in {
    gateReaper.inPremiereWindow(recordWithPremiere(Some(LocalDateTime.of(2026, 6, 13, 18, 0))), nowLocal) shouldBe false
  }
  it should "be false when the film has no showtimes at all" in {
    gateReaper.inPremiereWindow(recordWithPremiere(None), nowLocal) shouldBe false
  }

  // ── eligibility (independent of phase: assert over a whole period) ────────────

  "PremiereResolveReaper" should "re-try an in-window unresolved row exactly once per period" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "Bare")(identity) // tmdbId = None, premiere in window → unresolved + eligible
    runOnePeriod(new PremiereResolveReaper(cache, retry)).sum shouldBe 1
    seen.map(_.cleanTitle).distinct should contain only cache.keyOf("Bare", None).cleanTitle
  }

  it should "never re-try an unresolved row whose premiere is outside the window" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "FarOff", premiere = LocalDateTime.of(2026, 7, 20, 18, 0))(identity) // ~32 days out
    runOnePeriod(new PremiereResolveReaper(cache, retry)).sum shouldBe 0
    seen shouldBe empty
  }

  it should "never re-try a TMDB-resolved row" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "Resolved")(_.copy(tmdbId = Some(42)))
    runOnePeriod(new PremiereResolveReaper(cache, retry)).sum shouldBe 0
    seen shouldBe empty
  }

  it should "never re-try a row still awaiting detail enrichment" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "Pending")(_.copy(detailPending = true)) // unresolved but detail owed
    runOnePeriod(new PremiereResolveReaper(cache, retry)).sum shouldBe 0
    seen shouldBe empty
  }

  // ── the spread property ──────────────────────────────────────────────────────

  it should "spread a synchronized in-window backlog across the period rather than bursting" in {
    val cache = newCache(); val (_, retry) = recorder()
    val n = 150
    seedManyInWindow(cache, (0 until n).map(i => f"Stuck$i%03d"))
    val reaper = new PremiereResolveReaper(cache, retry)
    val perTick = runOnePeriod(reaper)

    perTick.sum shouldBe n                  // every eligible row re-tried exactly once over the period
    perTick.max should be <= 20             // no burst (the old daily sweep put all 150 in one tick)
    perTick.count(_ > 0) should be >= 100   // genuinely spread across most ticks
  }

  // ── recovery-burst cap ───────────────────────────────────────────────────────

  it should "cap re-tries per tick so a bunched backlog drains over several ticks" in {
    val cache = newCache(); val (_, retry) = recorder()
    seedManyInWindow(cache, (0 until 50).map(i => f"Stuck$i%03d"))
    // period == tickInterval → every eligible row is due on every tick, so the
    // cap is what bounds the batch (the leftover stays due next tick).
    val reaper = new PremiereResolveReaper(cache, retry,
      dueWindow = new DueWindow(PremiereResolveReaper.DefaultTickInterval), maxEnqueuePerTick = 10)
    reaper.tick(t0) shouldBe 10
    reaper.tick(t0 + PremiereResolveReaper.DefaultTickInterval.toMillis) shouldBe 10
  }

  it should "back off to throttledMaxEnqueuePerTick while the worker is CPU-credit throttled" in {
    val cache = newCache(); val (_, retry) = recorder()
    seedManyInWindow(cache, (0 until 50).map(i => f"Stuck$i%03d"))
    val throttled = new ScrapeThrottleSignal { def isThrottled = true; def slowScrapeMillis = 30000L }
    val reaper = new PremiereResolveReaper(cache, retry,
      dueWindow = new DueWindow(PremiereResolveReaper.DefaultTickInterval),
      maxEnqueuePerTick = 100, throttledMaxEnqueuePerTick = 5, throttle = throttled)
    reaper.tick(t0) shouldBe 5 // throttled → trickle (vs up to the healthy cap)
  }

  // ── cluster occurrence claim ─────────────────────────────────────────────────

  "PremiereResolveReaper.tickIfClaimed" should "not re-try when another machine claimed the window" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "Bare")(identity)
    new PremiereResolveReaper(cache, retry,
      dueWindow = new DueWindow(PremiereResolveReaper.DefaultTickInterval),
      runStore = NeverClaimScheduledRunStore, clock = clockAtT0).tickIfClaimed() shouldBe 0
    seen shouldBe empty
  }

  it should "re-try when it wins the claim" in {
    val cache = newCache(); val (_, retry) = recorder()
    seedRow(cache, "Bare")(identity)
    new PremiereResolveReaper(cache, retry,
      dueWindow = new DueWindow(PremiereResolveReaper.DefaultTickInterval),
      runStore = new InMemoryScheduledRunStore, clock = clockAtT0).tickIfClaimed() shouldBe 1
  }
}
