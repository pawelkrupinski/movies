package services.tasks

import models.{CinemaMovie, KinoApollo, Movie, Showtime}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository, InMemoryScreeningsRepository, InMemorySlotsRepository}
import services.cinemas.FakeDetailEnricher
import services.events.{DomainEvent, EventBus, InProcessEventBus, MovieDetailsComplete}
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

  /** A showtime that keeps its film currently screening. Relative to now, not a
   *  fixed date, so it cannot silently age into the past — which is what a future
   *  ended-film gate would then read every fixture here as. Far enough out to also
   *  sit ahead of the synthetic `t0` the phase-spread tests tick from. */
  private def screeningSoon = LocalDateTime.now().plusMonths(6)

  private class CapturingBus extends EventBus {
    val published = scala.collection.mutable.ListBuffer.empty[DomainEvent]
    def subscribe(handler: PartialFunction[DomainEvent, Unit]): Unit = ()
    def publish(event: DomainEvent): Unit = { published += event; () }
  }

  /** Seed the cache with one KinoApollo film carrying (optionally) a filmUrl —
   *  exactly what a bare deferred scrape persists. */
  private def cacheWith(filmUrl: Option[String]) = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus(), normalizer = titleNormalizer)
    val bare  = CinemaMovie(Movie("Dune"), KinoApollo, posterUrl = None, filmUrl = filmUrl,
      synopsis = None, cast = Seq.empty, director = Seq.empty,
      showtimes = Seq(Showtime(screeningSoon, Some("https://book"))))
    cache.recordCinemaScrape(KinoApollo, Seq(bare))
    cache
  }

  private def reaper(cache: CaffeineMovieCache, queue: InMemoryTaskQueue, fresh: InMemoryFreshnessStore,
                     bus: EventBus = new InProcessEventBus()) =
    new DetailReaper(Seq(enricher), cache, queue, fresh, bus)

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
                                                 slots      = Some(new InMemorySlotsRepository))
    val cache = new CaffeineMovieCache(repository, new InProcessEventBus(), normalizer = titleNormalizer)
    val bare  = CinemaMovie(Movie("Dune"), KinoApollo, posterUrl = None, filmUrl = filmUrl,
      synopsis = None, cast = Seq.empty, director = Seq.empty,
      showtimes = Seq(Showtime(screeningSoon, Some("https://book"))))
    cache.recordCinemaScrape(KinoApollo, Seq(bare))
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
    storedShowtimes(repository).count(_.isUpcoming(LocalDateTime.now())) shouldBe 1
    cachedShowtimes(cache) shouldBe empty
  }

  /** Seed the cache with `n` distinct deferred films, each carrying a filmUrl —
   *  a synchronized stale cohort, as a re-key / title-rule wave produces. */
  private def cacheWithMany(n: Int) = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus(), normalizer = titleNormalizer)
    val films = (1 to n).map { i =>
      CinemaMovie(Movie(s"Film $i"), KinoApollo, posterUrl = None, filmUrl = Some(s"http://ref/$i"),
        synopsis = None, cast = Seq.empty, director = Seq.empty,
        showtimes = Seq(Showtime(screeningSoon, Some("https://book"))))
    }
    cache.recordCinemaScrape(KinoApollo, films)
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
    val r = new DetailReaper(Seq(enricher), cache, queue, fresh, new InProcessEventBus(),
      dueWindow = new DueWindow(6.hours))
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

  // The OTHER half of the same gate. `detailOutstanding` gated on the same rule as
  // `tick`, so it too read "ended" for the whole corpus — and `reapStuckPending`
  // then released every detailPending row on every tick, publishing a spurious
  // MovieDetailsComplete for each. A future ended-film gate placed only here would
  // slip past the tick test above, so this pins it separately.
  it should "keep a detail-pending row that still owes a fetch when showtimes live in their own collection" in {
    val (cache, repository) = splitCacheWith(Some("http://ref"))
    val (queue, fresh)      = (new InMemoryTaskQueue, new InMemoryFreshnessStore)
    assertScreeningButStripped(cache, repository)

    val key = cache.keyOf("Dune", None)
    cache.putIfPresent(key, _.copy(detailPending = true))
    val bus = new CapturingBus
    reaper(cache, queue, fresh, bus).reapStuckPending() shouldBe 0
    cache.get(key).map(_.detailPending) shouldBe Some(true)
    bus.published shouldBe empty
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
    fresh.markFresh(EnrichDetailsTasks.dedupKey("kino-apollo", cache.keyOf("Dune", None)), FreshnessKind.DetailEnrich)
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
    val r = new DetailReaper(Seq(enricher), cacheWithMany(10), queue, fresh, new InProcessEventBus(),
      maxEnqueuePerTick = cap)
    r.tick() shouldBe 1   // cap = 1
    cap = 4
    r.tick() shouldBe 4   // live re-read picks up the new cap (a captured Int would still be 1)
  }

  it should "enqueue at most maxEnqueuePerTick details when a whole cohort is stale (anti-burst cap)" in {
    val (queue, fresh) = (new InMemoryTaskQueue, new InMemoryFreshnessStore)
    val r = new DetailReaper(Seq(enricher), cacheWithMany(5), queue, fresh, new InProcessEventBus(),
      maxEnqueuePerTick = 2)
    r.tick() shouldBe 2
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 2L
  }

  it should "drain the rest of the stale cohort over subsequent capped ticks" in {
    val (cache, queue, fresh) = (cacheWithMany(5), new InMemoryTaskQueue, new InMemoryFreshnessStore)
    val r = new DetailReaper(Seq(enricher), cache, queue, fresh, new InProcessEventBus(),
      maxEnqueuePerTick = 2)
    r.tick() shouldBe 2 // films 1–2
    r.tick() shouldBe 2 // 1–2 still waiting (deduped), next 2 fresh cohort members
    r.tick() shouldBe 1 // last one
    r.tick() shouldBe 0 // all five now waiting
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 5L
  }

  "DetailReaper.tickIfClaimed" should "not enqueue when another machine has claimed the occurrence" in {
    val (queue, fresh) = (new InMemoryTaskQueue, new InMemoryFreshnessStore)
    new DetailReaper(Seq(enricher), cacheWith(Some("http://ref")), queue, fresh, new InProcessEventBus(),
      runStore = NeverClaimScheduledRunStore).tickIfClaimed() shouldBe 0
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 0L
  }

  it should "tick when it wins the occurrence claim" in {
    val (queue, fresh) = (new InMemoryTaskQueue, new InMemoryFreshnessStore)
    new DetailReaper(Seq(enricher), cacheWith(Some("http://ref")), queue, fresh, new InProcessEventBus(),
      runStore = new InMemoryScheduledRunStore).tickIfClaimed() shouldBe 1
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
    new DetailReaper(Seq(enricher), cacheWith(Some("http://ref")), queue, hydrating, new InProcessEventBus(),
      runStore = new InMemoryScheduledRunStore).tickIfClaimed() shouldBe 0
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 0L
  }

  // ── reapStuckPending: release detail-pending rows that can never complete ────

  "DetailReaper.reapStuckPending" should
    "leave a row whose detail is still outstanding (filmUrl present, not yet fresh)" in {
    val (cache, queue, fresh) = (cacheWith(Some("http://ref")), new InMemoryTaskQueue, new InMemoryFreshnessStore)
    cache.putIfPresent(cache.keyOf("Dune", None), _.copy(detailPending = true))
    val bus = new CapturingBus
    reaper(cache, queue, fresh, bus).reapStuckPending() shouldBe 0
    cache.get(cache.keyOf("Dune", None)).map(_.detailPending) shouldBe Some(true) // still held back
    bus.published shouldBe empty
  }

  it should "release a detail-pending row with no deferred filmUrl to fetch (orphaned flag) and re-trigger TMDB" in {
    val (cache, queue, fresh) = (cacheWith(None), new InMemoryTaskQueue, new InMemoryFreshnessStore)
    cache.putIfPresent(cache.keyOf("Dune", None), _.copy(detailPending = true))
    val bus = new CapturingBus
    reaper(cache, queue, fresh, bus).reapStuckPending() shouldBe 1
    cache.get(cache.keyOf("Dune", None)).map(_.detailPending) shouldBe Some(false)
    bus.published.collect { case e: MovieDetailsComplete => e.title } shouldBe List("Dune")
  }

  it should "release a detail-pending Filmweb-fallback row (filmweb.pl filmUrl, no native detail to fetch) and re-trigger TMDB" in {
    val (cache, queue, fresh) = (cacheWith(Some(FilmwebShowtimesClient.filmPageUrl(1089))), new InMemoryTaskQueue, new InMemoryFreshnessStore)
    cache.putIfPresent(cache.keyOf("Dune", None), _.copy(detailPending = true))
    val bus = new CapturingBus
    reaper(cache, queue, fresh, bus).reapStuckPending() shouldBe 1
    cache.get(cache.keyOf("Dune", None)).map(_.detailPending) shouldBe Some(false)
    bus.published.collect { case e: MovieDetailsComplete => e.title } shouldBe List("Dune")
  }

  it should "release a detail-pending row whose detail is already fresh (a lost completion event)" in {
    val (cache, queue, fresh) = (cacheWith(Some("http://ref")), new InMemoryTaskQueue, new InMemoryFreshnessStore)
    cache.putIfPresent(cache.keyOf("Dune", None), _.copy(detailPending = true))
    fresh.markFresh(EnrichDetailsTasks.dedupKey("kino-apollo", cache.keyOf("Dune", None)), FreshnessKind.DetailEnrich)
    val bus = new CapturingBus
    reaper(cache, queue, fresh, bus).reapStuckPending() shouldBe 1
    cache.get(cache.keyOf("Dune", None)).map(_.detailPending) shouldBe Some(false)
    bus.published.size shouldBe 1
  }

  // The livelock this reaper drove in prod: a film whose detail page the cinema
  // took down after its run never got a freshness stamp, so it came due on EVERY
  // tick — the "Cinema City Enrichment" row ran at ~90% failures on two such
  // films, once a minute, indefinitely. Drives the real reaper→handler→reaper
  // cycle rather than asserting the stamp in isolation, because it is the second
  // tick going quiet that is the actual fix.
  it should "stop re-enqueueing a film whose detail page is durably gone, instead of once per tick" in {
    val (cache, queue, fresh) = (cacheWith(Some("http://ref")), new InMemoryTaskQueue, new InMemoryFreshnessStore)
    // ONE DueWindow instance across reaper and handler — they must agree on "due".
    val window = new DueWindow(6.hours)
    val gone   = new FakeDetailEnricher(KinoApollo, "kino-apollo",
      failure = Some(new HttpStatusException(404, "GET", "http://ref", None)))
    val r = new DetailReaper(Seq(gone), cache, queue, fresh, new InProcessEventBus(), dueWindow = window)
    val h = new EnrichDetailsHandler(Map("kino-apollo" -> gone), cache, fresh,
      new services.UptimeMonitor(), new InProcessEventBus(), window)

    r.tick() shouldBe 1
    // Run the task the way the worker does, so the queue is clear for the next tick
    // and only the freshness stamp can hold the film back.
    val task = queue.claim("worker", 1.minute, Instant.now()).getOrElse(fail("nothing queued"))
    h.handle(task) shouldBe HandlerOutcome.Done
    queue.complete(task.id, "worker")

    r.tick() shouldBe 0
    gone.calls shouldBe 1
  }

  it should "release a detail-pending row whose only detail page is durably gone, so it is not hidden forever" in {
    val (cache, queue, fresh) = (cacheWith(Some("http://ref")), new InMemoryTaskQueue, new InMemoryFreshnessStore)
    cache.putIfPresent(cache.keyOf("Dune", None), _.copy(detailPending = true))
    val window = new DueWindow(6.hours)
    val gone   = new FakeDetailEnricher(KinoApollo, "kino-apollo",
      failure = Some(new HttpStatusException(404, "GET", "http://ref", None)))
    val bus = new CapturingBus
    val r = new DetailReaper(Seq(gone), cache, queue, fresh, bus, dueWindow = window)
    val h = new EnrichDetailsHandler(Map("kino-apollo" -> gone), cache, fresh,
      new services.UptimeMonitor(), new InProcessEventBus(), window)

    // Before the detail is even attempted the row is legitimately outstanding.
    r.reapStuckPending() shouldBe 0
    r.tick() shouldBe 1
    val task = queue.claim("worker", 1.minute, Instant.now()).getOrElse(fail("nothing queued"))
    h.handle(task) shouldBe HandlerOutcome.Done

    // Now the detail is settled-unfetchable, so the row must reach the read model
    // rather than staying `detailPending` — invisible on the site — indefinitely.
    r.reapStuckPending() shouldBe 1
    cache.get(cache.keyOf("Dune", None)).map(_.detailPending) shouldBe Some(false)
    bus.published.collect { case e: MovieDetailsComplete => e.title } shouldBe List("Dune")
  }

  /** WHY THE DETAIL CACHE STILL EXISTS, expressed as the two numbers it lives
   *  between. The reaper re-enqueues an UNSTAMPED film every tick — and
   *  `DetailFetchOutcome.Failed` never stamps, including for a page that returns
   *  200 and parses to nothing (Kino Bulgarska: 1,438 failures to 56 successes in
   *  24h on one trailer-less film). `CachingDetailFetch` is what stops that
   *  once-a-minute retry becoming once-a-minute HTTP at a small cinema's site, so
   *  its TTL has to be many ticks long.
   *
   *  It must also expire well inside the refresh window, or the scheduled refresh
   *  is served from cache and cannot see a change — `CachingDetailFetchSpec` owns
   *  that half. Together the two say the TTL belongs strictly between the tick and
   *  the window, which is the thing to re-check if either is retuned. */
  it should "keep a detail cache TTL that absorbs the every-tick retry yet expires inside the refresh window" in {
    val tick   = DetailReaper.DefaultTickInterval
    val window = Freshness.ttlFor(FreshnessKind.DetailEnrich).getOrElse(fail("DetailEnrich lost its TTL"))
    val ttl    = CachingDetailFetch.DefaultTtl

    withClue(s"tick $tick, cache TTL $ttl, refresh window $window — ") {
      ttl should be > (tick * 10)   // a retry storm is absorbed, not passed through
      ttl should be < window        // a scheduled refresh is a real fetch
    }
  }
}
