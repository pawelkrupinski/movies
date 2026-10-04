package services.tasks

import models.{CinemaMovie, CinemaShowing, KinoApollo, Movie, MovieRecord, Showtime, SourceData}
import services.movies.{CacheKey, CaffeineMovieCache, InMemoryMovieRepository, InMemoryScreeningsRepository, InMemorySlotsRepository}
import models.Cinema
import services.cinemas.common.DetailEnricher
import services.cinemas.FakeDetailEnricher
import services.events.InProcessEventBus
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.schedule.{InMemoryScheduledRunStore, NeverClaimScheduledRunStore}
import services.freshness.{Freshness, FreshnessKind, InMemoryFreshnessStore}
import services.cinemas.pl.FilmwebShowtimesClient
import tools.{CachingDetailFetch, HttpStatusException}

import java.time.{Instant, LocalDateTime}
import scala.concurrent.duration._
import services.movies.SingleCountryNormalizer.titleNormalizer

class DetailReaperSpec extends AnyFlatSpec with Matchers {

  private val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo")

  /** The one instant every cache and reaper here runs at. */
  private val specClock = java.time.Clock.fixed(Instant.parse("2026-06-01T10:00:00Z"), java.time.ZoneOffset.UTC)

  /** A showtime that keeps its film currently screening. Relative to [[specClock]],
   *  which the caches read too, so it cannot age into the past — which is what a
   *  future ended-film gate would then read every fixture here as. Far enough out to
   *  also sit ahead of the synthetic `t0` the phase-spread tests tick from. */
  private def screeningSoon = LocalDateTime.now(specClock).plusMonths(6)


  /** Seed the cache with one KinoApollo film carrying (optionally) a filmUrl —
   *  exactly what a bare deferred scrape persists. */
  private def cacheWith(filmUrl: Option[String]) = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val bare  = CinemaMovie(Movie("Dune"), KinoApollo, posterUrl = None, filmUrl = filmUrl,
      synopsis = None, cast = Seq.empty, director = Seq.empty,
      showtimes = Seq(Showtime(screeningSoon, Some("https://book"))))
    services.movies.ListingSeed.land(cache, KinoApollo, Seq(bare))
    cache
  }

  private def reaper(cache: CaffeineMovieCache, queue: InMemoryTaskQueue, fresh: InMemoryFreshnessStore) =
    new DetailReaper(Seq(enricher), cache, queue, fresh, clock = specClock)

  // The tick asked every cached film for its detail pages every minute, re-deriving each film's
  // venues (`cinemaData`) though almost none had changed — 2.6% of the UK worker's CPU (JFR
  // 2026-10-01). A film the cache still holds as the same record keeps the pages it was asked.
  "A detail tick" should "derive again only the films whose cached record changed" in {
    var derived = 0
    val counting = new DetailPages {
      def of(key: CacheKey, record: MovieRecord, enrichersByCinema: Map[Cinema, Seq[DetailEnricher]]) = {
        derived += 1; DetailPages.PerVenue.of(key, record, enrichersByCinema)
      }
    }
    val cache  = cacheWith(Some("http://kinoapollo/dune"))
    val queue  = new InMemoryTaskQueue
    val r      = new DetailReaper(Seq(enricher), cache, queue, new InMemoryFreshnessStore,
      pages = counting, clock = specClock)
    r.tick() shouldBe 1
    derived shouldBe 1
    r.tick()
    derived shouldBe 1                                   // the same record: its pages are remembered
    services.movies.ListingSeed.land(cache, KinoApollo, Seq(CinemaMovie(Movie("Dune"), KinoApollo, posterUrl = None,
      filmUrl = Some("http://kinoapollo/dune-2"), synopsis = None, cast = Seq.empty, director = Seq.empty,
      showtimes = Seq(Showtime(screeningSoon, Some("https://book"))))))
    r.tick()
    derived shouldBe 2                                   // the record moved: derived again
  }

  /** The same seed as [[cacheWith]], but stored PRODUCTION's way: showtimes in
   *  `screenings`, slots in `movie_slots`. Every other fixture here wires a bare
   *  `InMemoryMovieRepository` — the one shape that keeps showtime lists resident —
   *  so a reaper rule that reads `SourceData.showtimes` passes all of them while
   *  being dead on a real worker, because `CaffeineMovieCache.forCache` strips those
   *  lists to `Nil` the moment `repository.hasScreenings` is true. Cf.
   *  [[services.movies.DepthGuardUnderSplitSpec]], where the same divergence silently
   *  disabled the degraded-scrape depth guard. */
  private def splitCacheWith(filmUrl: Option[String]) = {
    val repository = new InMemoryMovieRepository(screenings = Some(new InMemoryScreeningsRepository),
                                                 slots      = Some(new InMemorySlotsRepository), normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val bare  = CinemaMovie(Movie("Dune"), KinoApollo, posterUrl = None, filmUrl = filmUrl,
      synopsis = None, cast = Seq.empty, director = Seq.empty,
      showtimes = Seq(Showtime(screeningSoon, Some("https://book"))))
    services.movies.ListingSeed.land(cache, KinoApollo, Seq(bare))
    (cache, repository)
  }

  /** The showtimes the film actually HAS — read from storage, since under the split
   *  that is the only place they survive. */
  private def storedShowtimes(repository: InMemoryMovieRepository): Seq[Showtime] =
    repository.findAll().flatMap(_.record.data.values).flatMap(_.showtimes).toSeq

  /** The showtimes the reaper can see — it walks `cache.entries`, and every resident
   *  slot has been through `ShowtimesDigest.stripForCache`. */
  private def cachedShowtimes(cache: CaffeineMovieCache): Seq[Showtime] =
    cache.entries.flatMap(_._2.data.values).flatMap(_.showtimes).toSeq

  /** The split's defining asymmetry, asserted before each test that depends on it:
   *  the film IS screening — one upcoming showtime, stored — and the cache the
   *  reaper walks holds NONE of it. Pinning both halves is the point: if
   *  `stripForCache` ever stops stripping, these fixtures quietly stop covering the
   *  thing they exist to cover, and only this assertion would say so. */
  private def assertScreeningButStripped(cache: CaffeineMovieCache, repository: InMemoryMovieRepository): Unit = {
    storedShowtimes(repository).count(_.isUpcoming(LocalDateTime.now(specClock))) shouldBe 1
    cachedShowtimes(cache) shouldBe empty
  }

  /** Seed the cache with `n` distinct deferred films, each carrying a filmUrl —
   *  a synchronized stale cohort, as a re-key / title-rule wave produces. */
  private def cacheWithMany(n: Int) = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val films = (1 to n).map { i =>
      CinemaMovie(Movie(s"Film $i"), KinoApollo, posterUrl = None, filmUrl = Some(s"http://ref/$i"),
        synopsis = None, cast = Seq.empty, director = Seq.empty,
        showtimes = Seq(Showtime(screeningSoon, Some("https://book"))))
    }
    services.movies.ListingSeed.land(cache, KinoApollo, films)
    cache
  }

  /** Drive `n` deferred films — all stamped detail-fresh at the SAME instant (a
   *  synchronized cohort) — through one full 6h period of ticks spaced `delta`
   *  apart, returning the per-tick enqueue counts. The phase spread should smear
   *  them across the ticks; a finer `delta` flattens the worst-case tick. */
  private def perTickOverPeriod(n: Int, delta: FiniteDuration): Seq[Int] = {
    val t0 = Instant.parse("2026-06-18T00:00:00Z").toEpochMilli
    val cache = cacheWithMany(n)
    val (queue, fresh) = (new InMemoryTaskQueue, new InMemoryFreshnessStore)
    (1 to n).foreach { i =>
      fresh.markFresh(EnrichDetailsTasks.dedupKey("kino-apollo", cache.keyOf(s"Film $i", None)),
        FreshnessKind.DetailEnrich, Instant.ofEpochMilli(t0))
    }
    val r = new DetailReaper(Seq(enricher), cache, queue, fresh,
      dueWindow = new DueWindow(6.hours), clock = specClock)
    val ticks = (6.hours.toMillis / delta.toMillis).toInt
    (1 to ticks).map(k => r.tick(t0 + k * delta.toMillis))
  }

  // The actual smoothing lever for the prod `EnrichDetails` spikes: a tick interval
  // `delta` only catches the rows whose phase boundary fell in the last `delta`, so
  // the per-tick burst scales with `delta`. Production wires
  // `DetailReaper.DefaultTickInterval`; this guards it's finer than the old 5-min
  // cadence — a finer default genuinely flattens the worst-case per-tick burst for
  // the same synchronized cohort. (Fails when the default IS 5min: the two runs are
  // identical, so the finer-run max isn't materially below the 5-min max.)
  "DetailReaper" should "keep the per-tick burst materially flatter at the default interval than at the old 5-min cadence" in {
    val n = 240
    val coarseMax  = perTickOverPeriod(n, delta = 5.minutes).max
    val defaultMax = perTickOverPeriod(n, delta = DetailReaper.DefaultTickInterval).max
    DetailReaper.DefaultTickInterval should be < (5.minutes: FiniteDuration)
    defaultMax.toDouble should be <= (coarseMax / 2.0)
  }

  "DetailReaper.tick" should "enqueue a detail task for each deferred film that has a filmUrl and isn't fresh" in {
    val (queue, fresh) = (new InMemoryTaskQueue, new InMemoryFreshnessStore)
    reaper(cacheWith(Some("http://ref")), queue, fresh).tick() shouldBe 1
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L
  }

  /** A cut-over worker's reaper asks every page a venue slot names, keyed by the page: a film row the
   *  identity model gathered two of a venue's listings on — each with its own page — needs both, and
   *  which one a per-venue reaper asked depended on arrival order (Identity model convergence P1). The
   *  pipeline keeps one page per venue and film. */
  it should "ask one page per venue and film by default, and every page a venue slot names per page" in {
    def cacheWithTwoPages = {
      val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
      cache.put(cache.keyOf("Dune", None), MovieRecord(data = Map(
        CinemaShowing(KinoApollo, "dune")         -> SourceData(title = Some("Dune"), filmUrl = Some("http://ref/dune")),
        CinemaShowing(KinoApollo, "dunesingalong") -> SourceData(title = Some("Dune sing-along"), filmUrl = Some("http://ref/dune-sing-along")))))
      cache
    }
    val (perVenue, perPage) = (new InMemoryTaskQueue, new InMemoryTaskQueue)
    new DetailReaper(Seq(enricher), cacheWithTwoPages, perVenue, new InMemoryFreshnessStore, clock = specClock).tick() shouldBe 1
    new DetailReaper(Seq(enricher), cacheWithTwoPages, perPage, new InMemoryFreshnessStore, clock = specClock,
      pages = DetailPages.PerPage).tick() shouldBe 2
  }

  // The regression that took EVERY cinema's detail enrichment down for 16h on
  // 2026-08-03 (deployed 14:19Z, last detail freshness stamp of any group 14:18Z).
  // A `stillScreening` gate was added to `tick` to stop refreshing ended films, but
  // it asked `SourceData.showtimes` — which the read-split strips off every
  // cache-resident record — so it read "ended" for the entire live corpus and the
  // reaper enqueued nothing, ever. This is the only fixture here stored the way
  // production stores; any future ended-film gate has to keep it green.
  it should "enqueue a due detail when showtimes live in their own collection, as production stores them" in {
    val (cache, repository) = splitCacheWith(Some("http://ref"))
    val (queue, fresh)      = (new InMemoryTaskQueue, new InMemoryFreshnessStore)
    assertScreeningButStripped(cache, repository)

    reaper(cache, queue, fresh).tick() shouldBe 1
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L
  }

  it should "skip a film with no filmUrl (no detail reference to fetch)" in {
    val (queue, fresh) = (new InMemoryTaskQueue, new InMemoryFreshnessStore)
    reaper(cacheWith(None), queue, fresh).tick() shouldBe 0
  }

  it should "skip a Filmweb-fallback row whose filmUrl is a filmweb.pl page the native enricher can't fetch" in {
    val (queue, fresh) = (new InMemoryTaskQueue, new InMemoryFreshnessStore)
    reaper(cacheWith(Some(FilmwebShowtimesClient.filmPageUrl(1089))), queue, fresh).tick() shouldBe 0
  }

  it should "skip a film whose detail is already fresh" in {
    val (cache, queue, fresh) = (cacheWith(Some("http://ref")), new InMemoryTaskQueue, new InMemoryFreshnessStore)
    fresh.markFresh(EnrichDetailsTasks.dedupKey("kino-apollo", cache.keyOf("Dune", None)), FreshnessKind.DetailEnrich, specClock.instant())
    reaper(cache, queue, fresh).tick() shouldBe 0
  }

  it should "not double-enqueue across consecutive ticks (the queue dedups the still-waiting task)" in {
    val (cache, queue, fresh) = (cacheWith(Some("http://ref")), new InMemoryTaskQueue, new InMemoryFreshnessStore)
    val r = reaper(cache, queue, fresh)
    r.tick() shouldBe 1
    r.tick() shouldBe 0 // already waiting → unique index rejects the duplicate
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L
  }

  it should "read maxEnqueuePerTick live each tick, so an /admin/config cap flip applies mid-flight" in {
    val (queue, fresh) = (new InMemoryTaskQueue, new InMemoryFreshnessStore)
    var cap = 1
    val r = new DetailReaper(Seq(enricher), cacheWithMany(10), queue, fresh,
      maxEnqueuePerTick = settings.DetailMaxEnqueuePerTick(cap), clock = specClock)
    r.tick() shouldBe 1   // cap = 1
    cap = 4
    r.tick() shouldBe 4   // live re-read picks up the new cap (a captured Int would still be 1)
  }

  it should "enqueue at most maxEnqueuePerTick details when a whole cohort is stale (anti-burst cap)" in {
    val (queue, fresh) = (new InMemoryTaskQueue, new InMemoryFreshnessStore)
    val r = new DetailReaper(Seq(enricher), cacheWithMany(5), queue, fresh,
      maxEnqueuePerTick = settings.DetailMaxEnqueuePerTick(2), clock = specClock)
    r.tick() shouldBe 2
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 2L
  }

  it should "drain the rest of the stale cohort over subsequent capped ticks" in {
    val (cache, queue, fresh) = (cacheWithMany(5), new InMemoryTaskQueue, new InMemoryFreshnessStore)
    val r = new DetailReaper(Seq(enricher), cache, queue, fresh,
      maxEnqueuePerTick = settings.DetailMaxEnqueuePerTick(2), clock = specClock)
    r.tick() shouldBe 2 // films 1–2
    r.tick() shouldBe 2 // 1–2 still waiting (deduped), next 2 fresh cohort members
    r.tick() shouldBe 1 // last one
    r.tick() shouldBe 0 // all five now waiting
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 5L
  }

  "DetailReaper.tickIfClaimed" should "not enqueue when another machine has claimed the occurrence" in {
    val (queue, fresh) = (new InMemoryTaskQueue, new InMemoryFreshnessStore)
    new DetailReaper(Seq(enricher), cacheWith(Some("http://ref")), queue, fresh,
      runStore = NeverClaimScheduledRunStore, clock = specClock).tickIfClaimed() shouldBe 0
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 0L
  }

  it should "tick when it wins the occurrence claim" in {
    val (queue, fresh) = (new InMemoryTaskQueue, new InMemoryFreshnessStore)
    new DetailReaper(Seq(enricher), cacheWith(Some("http://ref")), queue, fresh,
      runStore = new InMemoryScheduledRunStore, clock = specClock).tickIfClaimed() shouldBe 1
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L
  }

  it should "hold its tick (enqueue nothing) while the detail freshness mirror is still hydrating" in {
    // A never-fresh film with a filmUrl is due. But the detail stamps hydrate in
    // the rest phase, so until they land the reaper must NOT read the empty mirror
    // as "every detail stale" and re-enqueue the whole deferred-detail corpus — the
    // recurring per-deploy spike. It wins the claim yet holds until ready.
    val (queue, fresh) = (new InMemoryTaskQueue, new InMemoryFreshnessStore)
    val hydrating = new InMemoryFreshnessStore {
      override def whenReady(kind: FreshnessKind): scala.concurrent.Future[Unit] = scala.concurrent.Promise[Unit]().future
    }
    new DetailReaper(Seq(enricher), cacheWith(Some("http://ref")), queue, hydrating,
      runStore = new InMemoryScheduledRunStore, clock = specClock).tickIfClaimed() shouldBe 0
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 0L
  }

  // The livelock this reaper drove in prod: a film whose detail page the cinema
  // took down after its run never got a freshness stamp, so it came due on EVERY
  // tick — the "Cinema City Enrichment" row ran at ~90% failures on two such
  // films, once a minute, indefinitely. Drives the real reaper→handler→reaper
  // cycle rather than asserting the stamp in isolation, because it is the second
  // tick going quiet that is the actual fix.
  "DetailReaper" should "stop re-enqueueing a film whose detail page is durably gone, instead of once per tick" in {
    val (cache, queue, fresh) = (cacheWith(Some("http://ref")), new InMemoryTaskQueue, new InMemoryFreshnessStore)
    // ONE DueWindow instance across reaper and handler — they must agree on "due".
    val window = new DueWindow(6.hours)
    val gone   = new FakeDetailEnricher(KinoApollo, "kino-apollo",
      failure = Some(new HttpStatusException(404, "GET", "http://ref", None)))
    val r = new DetailReaper(Seq(gone), cache, queue, fresh, dueWindow = window, clock = specClock)
    val h = new EnrichDetailsHandler(Map("kino-apollo" -> gone), cache, fresh,
      new services.UptimeMonitor(clock = _root_.tools.SpecClock.Pinned), new InProcessEventBus(), window, clock = _root_.tools.SpecClock.Pinned, enrichmentLanguage = models.Country.Poland.language)

    r.tick() shouldBe 1
    // Run the task the way the worker does, so the queue is clear for the next tick
    // and only the freshness stamp can hold the film back.
    val task = queue.claim("worker", 1.minute, specClock.instant()).getOrElse(fail("nothing queued"))
    h.handle(task) shouldBe HandlerOutcome.Done
    queue.complete(task.id, "worker")

    r.tick() shouldBe 0
    gone.calls shouldBe 1
  }

  /** THE DETAIL CACHE'S TTL LIVES BETWEEN TWO NUMBERS, and this is the lower one.
   *
   *  It has to expire well inside the refresh window or a scheduled refresh is
   *  served from cache and cannot see a change — `CachingDetailFetchSpec` owns
   *  that half. It also has to outlive many reaper ticks, because the reaper
   *  re-enqueues an UNSTAMPED film every tick and a cache that expires faster than
   *  the work arrives is not a cache at all.
   *
   *  The sharpest case for that floor is gone rather than hypothetical: a page
   *  returning 200 and parsing to nothing used to become `Failed`, which never
   *  stamps, so one trailer-less film cost Kino Bulgarska 1,438 failures to 56
   *  successes in 24h, and this cache was what kept the retries off the venue.
   *  `DetailEnricherDurableFailureSpec` now holds every client to treating a page
   *  that LOADED as a detail, so the floor guards a regression rather than a live
   *  wound — which is the reason to keep it, not to drop it. */
  it should "keep a detail cache TTL that absorbs the every-tick retry yet expires inside the refresh window" in {
    val tick   = DetailReaper.DefaultTickInterval
    val window = Freshness.ttlFor(FreshnessKind.DetailEnrich).getOrElse(fail("DetailEnrich lost its TTL"))
    val ttl    = CachingDetailFetch.DefaultTtl

    withClue(s"tick $tick, cache TTL $ttl, refresh window $window — ") {
      ttl should be > (tick * 10)   // outlives the work arriving at it
      ttl should be < window        // a scheduled refresh is a real fetch
    }
  }
}
