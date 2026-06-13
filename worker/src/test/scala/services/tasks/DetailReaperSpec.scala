package services.tasks

import models.{CinemaMovie, KinoApollo, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.FakeDetailEnricher
import services.events.{DomainEvent, EventBus, InProcessEventBus, MovieDetailsComplete}
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepo}

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
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo(), new InProcessEventBus())
    val bare  = CinemaMovie(Movie("Dune"), KinoApollo, posterUrl = None, filmUrl = filmUrl,
      synopsis = None, cast = Seq.empty, director = Seq.empty,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 7, 18, 0), Some("https://book"))))
    cache.recordCinemaScrape(KinoApollo, Seq(bare))
    cache
  }

  private def reaper(cache: CaffeineMovieCache, queue: InMemoryTaskQueue, fresh: InMemoryFreshnessStore,
                     bus: EventBus = new InProcessEventBus()) =
    new DetailReaper(Seq(enricher), cache, queue, fresh, bus)

  "DetailReaper.tick" should "enqueue a detail task for each deferred film that has a filmUrl and isn't fresh" in {
    val (queue, fresh) = (new InMemoryTaskQueue, new InMemoryFreshnessStore)
    reaper(cacheWith(Some("http://ref")), queue, fresh).tick() shouldBe 1
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L
  }

  it should "skip a film with no filmUrl (no detail reference to fetch)" in {
    val (queue, fresh) = (new InMemoryTaskQueue, new InMemoryFreshnessStore)
    reaper(cacheWith(None), queue, fresh).tick() shouldBe 0
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
