package services.tasks

import models.{CinemaMovie, KinoApollo, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{FakeDetailEnricher, FilmwebShowtimesClient}
import services.events.{DomainEvent, EventBus, InProcessEventBus, MovieDetailsComplete}
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import services.schedule.{InMemoryScheduledRunStore, NeverClaimScheduledRunStore}

import java.time.LocalDateTime

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
