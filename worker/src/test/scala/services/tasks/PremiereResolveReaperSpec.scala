package services.tasks

import models.{CinemaMovie, KinoApollo, Movie, MovieRecord, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.movies.{CacheKey, CaffeineMovieCache, InMemoryMovieRepository}
import services.schedule.{AlwaysClaimScheduledRunStore, InMemoryScheduledRunStore, NeverClaimScheduledRunStore, ScheduledRunStore}

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}
import scala.collection.mutable
import scala.concurrent.duration._

class PremiereResolveReaperSpec extends AnyFlatSpec with Matchers {

  private val t0    = Instant.parse("2026-06-18T00:00:00Z").toEpochMilli
  private val now   = LocalDateTime.ofInstant(Instant.ofEpochMilli(t0), ZoneOffset.UTC) // 2026-06-18T00:00
  private val fixed = Clock.fixed(Instant.ofEpochMilli(t0), ZoneOffset.UTC)             // pins tickIfClaimed's now

  private def newCache() = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus())

  /** Seed an unresolved row whose premiere (its first screening) is
   *  `premiereDaysFromNow` from t0 — `persist` derives `firstScreeningDate`
   *  from the seeded showtime. */
  private def seedRow(cache: CaffeineMovieCache, title: String, premiereDaysFromNow: Long)(edit: MovieRecord => MovieRecord): Unit = {
    cache.recordCinemaScrape(KinoApollo, Seq(CinemaMovie(
      Movie(title), KinoApollo, None, None, None, Seq.empty, Seq.empty,
      Seq(Showtime(now.plusDays(premiereDaysFromNow), Some("https://book"))))))
    cache.putIfPresent(cache.keyOf(title, None), edit)
  }

  /** A recording retry seam — the reaper owns the schedule/gate; the per-row
   *  re-try (clear negative + dispatch) is `MovieService.retryResolve` in prod. */
  private def recorder(): (mutable.Buffer[CacheKey], CacheKey => Unit) = {
    val seen = mutable.Buffer.empty[CacheKey]
    (seen, key => { seen += key; () })
  }

  /** A reaper pinned to UTC (and a fixed clock) so the premiere-window math reads
   *  directly off t0 for both `tick(nowMillis)` and `tickIfClaimed`. */
  private def newReaper(cache: CaffeineMovieCache, retry: CacheKey => Unit,
                        dueWindow: DueWindow = new DueWindow(24.hours),
                        maxEnqueuePerTick: Int = Int.MaxValue,
                        throttledMaxEnqueuePerTick: Int = Int.MaxValue,
                        throttle: ScrapeThrottleSignal = ScrapeThrottleSignal.AlwaysHealthy,
                        runStore: ScheduledRunStore = AlwaysClaimScheduledRunStore) =
    new PremiereResolveReaper(cache, retry, zone = ZoneOffset.UTC, dueWindow = dueWindow,
      maxEnqueuePerTick = maxEnqueuePerTick, throttledMaxEnqueuePerTick = throttledMaxEnqueuePerTick,
      throttle = throttle, runStore = runStore, clock = fixed)

  /** Drive a full period of contiguous 5-min ticks, returning per-tick re-try counts. */
  private def runOnePeriod(reaper: PremiereResolveReaper, period: FiniteDuration = 24.hours): Seq[Int] = {
    val delta = PremiereResolveReaper.DefaultTickInterval
    val ticks = (period.toMillis / delta.toMillis).toInt
    (1 to ticks).map(k => reaper.tick(t0 + k * delta.toMillis))
  }

  // ── premiere-window eligibility (asserted over a whole period — phase-independent) ──

  "PremiereResolveReaper" should "re-try an unresolved row whose premiere is within the lead window" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "SoonPremiere", premiereDaysFromNow = 3)(identity) // 3 days out → inside the 7-day lead
    runOnePeriod(newReaper(cache, retry)).sum shouldBe 1
    seen.map(_.cleanTitle).distinct should contain only cache.keyOf("SoonPremiere", None).cleanTitle
  }

  it should "never re-try a row whose premiere is still beyond the lead window" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "FarOff", premiereDaysFromNow = 30)(identity) // 30 days out → outside the 7-day lead
    runOnePeriod(newReaper(cache, retry)).sum shouldBe 0
    seen shouldBe empty
  }

  it should "never re-try a row whose premiere is in the past beyond the grace window" in {
    val cache = newCache(); val (_, retry) = recorder()
    seedRow(cache, "LongGone", premiereDaysFromNow = -5)(identity) // premiered 5 days ago → past the 1-day grace
    runOnePeriod(newReaper(cache, retry)).sum shouldBe 0
  }

  it should "never re-try a row with no known first-screening date" in {
    val cache = newCache(); val (_, retry) = recorder()
    cache.put(cache.keyOf("NoDate", None), MovieRecord()) // no showtimes → firstScreeningDate stays None
    runOnePeriod(newReaper(cache, retry)).sum shouldBe 0
  }

  it should "never re-try a TMDB-resolved row even inside its premiere window" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "Resolved", premiereDaysFromNow = 3)(_.copy(tmdbId = Some(42)))
    runOnePeriod(newReaper(cache, retry)).sum shouldBe 0
    seen shouldBe empty
  }

  it should "never re-try a row still awaiting detail enrichment" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "Pending", premiereDaysFromNow = 3)(_.copy(detailPending = true)) // unresolved but detail owed
    runOnePeriod(newReaper(cache, retry)).sum shouldBe 0
    seen shouldBe empty
  }

  // ── the spread property (a synchronized in-window cohort trickles out) ──

  it should "spread a synchronized in-window backlog across the period rather than bursting" in {
    val cache = newCache(); val (_, retry) = recorder()
    val n = 150
    (0 until n).foreach(i => seedRow(cache, f"Stuck$i%03d", premiereDaysFromNow = 3)(identity)) // all in-window, "born" together
    val perTick = runOnePeriod(newReaper(cache, retry))

    perTick.sum shouldBe n                  // every in-window row re-tried exactly once over the period
    perTick.max should be <= 20             // no burst
    perTick.count(_ > 0) should be >= 100   // genuinely spread across most ticks
  }

  // ── recovery-burst cap ──

  it should "cap re-tries per tick so a bunched in-window backlog drains over several ticks" in {
    val cache = newCache(); val (_, retry) = recorder()
    (0 until 50).foreach(i => seedRow(cache, f"Stuck$i%03d", premiereDaysFromNow = 3)(identity))
    // period == tickInterval → every in-window row is due on every tick, so the
    // cap is what bounds the batch (the leftover stays due next tick).
    val reaper = newReaper(cache, retry,
      dueWindow = new DueWindow(PremiereResolveReaper.DefaultTickInterval), maxEnqueuePerTick = 10)
    reaper.tick(t0) shouldBe 10
    reaper.tick(t0 + PremiereResolveReaper.DefaultTickInterval.toMillis) shouldBe 10
  }

  it should "back off to throttledMaxEnqueuePerTick while the worker is CPU-credit throttled" in {
    val cache = newCache(); val (_, retry) = recorder()
    (0 until 50).foreach(i => seedRow(cache, f"Stuck$i%03d", premiereDaysFromNow = 3)(identity))
    val throttled = new ScrapeThrottleSignal { def isThrottled = true; def slowScrapeMillis = 30000L }
    val reaper = newReaper(cache, retry,
      dueWindow = new DueWindow(PremiereResolveReaper.DefaultTickInterval),
      maxEnqueuePerTick = 100, throttledMaxEnqueuePerTick = 5, throttle = throttled)
    reaper.tick(t0) shouldBe 5 // throttled → trickle (vs up to the healthy cap)
  }

  // ── cluster occurrence claim ──

  "PremiereResolveReaper.tickIfClaimed" should "not re-try when another machine claimed the window" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "Bare", premiereDaysFromNow = 3)(identity)
    newReaper(cache, retry,
      dueWindow = new DueWindow(PremiereResolveReaper.DefaultTickInterval),
      runStore = NeverClaimScheduledRunStore).tickIfClaimed() shouldBe 0
    seen shouldBe empty
  }

  it should "re-try when it wins the claim" in {
    val cache = newCache(); val (_, retry) = recorder()
    seedRow(cache, "Bare", premiereDaysFromNow = 3)(identity)
    newReaper(cache, retry,
      dueWindow = new DueWindow(PremiereResolveReaper.DefaultTickInterval),
      runStore = new InMemoryScheduledRunStore).tickIfClaimed() shouldBe 1
  }
}
