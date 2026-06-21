package services.tasks

import models.{CinemaMovie, KinoApollo, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{FakeDetailEnricher, FilmwebShowtimesClient}
import services.events.{DomainEvent, EventBus, InProcessEventBus, MovieDetailsComplete}
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import services.schedule.{InMemoryScheduledRunStore, NeverClaimScheduledRunStore}

import java.time.{Instant, LocalDateTime}
import scala.concurrent.duration._

class DetailReaperSpec extends AnyFlatSpec with Matchers {

  private val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo")

  private class CapturingBus extends EventBus {
    val published = scala.collection.mutable.ListBuffer.empty[DomainEvent]
    def subscribe(handler: PartialFunction[DomainEvent, Unit]): Unit = ()
    def publish(event: DomainEvent): Unit = { published += event; () }
  }

  /** Seed the cache with one KinoApollo film carrying (optionally) a filmUrl —
   *  exactly what a bare deferred scrape persists. */
  private def cacheWith(filmUrl: Option[String]) = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus())
    val bare  = CinemaMovie(Movie("Dune"), KinoApollo, posterUrl = None, filmUrl = filmUrl,
      synopsis = None, cast = Seq.empty, director = Seq.empty,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 7, 18, 0), Some("https://book"))))
    cache.recordCinemaScrape(KinoApollo, Seq(bare))
    cache
  }

  private def reaper(cache: CaffeineMovieCache, queue: InMemoryTaskQueue, fresh: InMemoryFreshnessStore,
                     bus: EventBus = new InProcessEventBus()) =
    new DetailReaper(Seq(enricher), cache, queue, fresh, bus)

  /** Seed the cache with `n` distinct deferred films, each carrying a filmUrl —
   *  a synchronized stale cohort, as a re-key / title-rule wave produces. */
  private def cacheWithMany(n: Int) = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus())
    val films = (1 to n).map { i =>
      CinemaMovie(Movie(s"Film $i"), KinoApollo, posterUrl = None, filmUrl = Some(s"http://ref/$i"),
        synopsis = None, cast = Seq.empty, director = Seq.empty,
        showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 7, 18, 0), Some("https://book"))))
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

  it should "back off to throttledMaxEnqueuePerTick while the worker is CPU-credit throttled" in {
    val (queue, fresh) = (new InMemoryTaskQueue, new InMemoryFreshnessStore)
    val throttled = new ScrapeThrottleSignal { def isThrottled = true; def ewmaMillis = 30000L }
    val r = new DetailReaper(Seq(enricher), cacheWithMany(5), queue, fresh, new InProcessEventBus(),
      maxEnqueuePerTick = 50, throttledMaxEnqueuePerTick = 2, throttle = throttled)
    r.tick() shouldBe 2 // throttled → trickle (vs all 5 at the healthy cap)
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
}
