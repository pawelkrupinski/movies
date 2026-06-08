package services.tasks

import models.{CinemaMovie, KinoApollo, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{DetailEnricher, FakeDetailEnricher, FilmDetail}
import services.events.InProcessEventBus
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepo}

import java.time.LocalDateTime

class EnrichDetailsHandlerSpec extends AnyFlatSpec with Matchers {
  import HandlerOutcome._

  /** A cache pre-seeded with one (KinoApollo, title) row whose slot carries
   *  showtimes but no detail — exactly what a bare scrape leaves behind. */
  private def seededCache(title: String) = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo(), new InProcessEventBus())
    val bare = CinemaMovie(Movie(title), KinoApollo, posterUrl = None, filmUrl = Some("http://ref"),
      synopsis = None, cast = Seq.empty, director = Seq.empty,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 7, 18, 0), Some("https://book"))))
    cache.recordCinemaScrape(KinoApollo, Seq(bare))
    cache
  }

  private def taskFor(group: String, cache: CaffeineMovieCache, title: String, enricher: DetailEnricher) = {
    val key = cache.keyOf(title, None)
    Task("id", TaskType.EnrichDetails, EnrichDetailsTasks.dedupKey(group, key),
      EnrichDetailsTasks.payload(enricher, key, "http://ref"), attempts = 1)
  }

  "EnrichDetailsHandler" should "merge fetched detail into the cinema slot, preserving showtimes, and mark fresh" in {
    val cache    = seededCache("Dune")
    val fresh    = new InMemoryFreshnessStore
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(FilmDetail(synopsis = Some("A great film"), cast = Seq("Zendaya"))))
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, fresh)
    val task     = taskFor("kino-apollo", cache, "Dune", enricher)

    h.handle(task) shouldBe Done
    val slot = cache.get(cache.keyOf("Dune", None)).flatMap(_.data.get(KinoApollo))
    slot.flatMap(_.synopsis) shouldBe Some("A great film")
    slot.map(_.cast)         shouldBe Some(Seq("Zendaya"))
    slot.map(_.showtimes.size) shouldBe Some(1) // showtimes from the scrape preserved
    fresh.isFresh(task.dedupKey, FreshnessKind.DetailEnrich) shouldBe true
  }

  it should "skip without fetching when the detail is already fresh" in {
    val cache    = seededCache("Dune")
    val fresh    = new InMemoryFreshnessStore
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(FilmDetail(synopsis = Some("x"))))
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, fresh)
    val task     = taskFor("kino-apollo", cache, "Dune", enricher)
    fresh.markFresh(task.dedupKey, FreshnessKind.DetailEnrich)

    h.handle(task) shouldBe Skipped
    enricher.calls shouldBe 0
  }

  it should "drop a task whose detail group has no enricher" in {
    val cache = seededCache("Dune")
    val h     = new EnrichDetailsHandler(Map.empty, cache, new InMemoryFreshnessStore)
    val task  = taskFor("gone", cache, "Dune", new FakeDetailEnricher(KinoApollo, "gone", None))
    h.handle(task) shouldBe Done
  }

  it should "report Done but stay stale when the fetch yields nothing (so the next scrape retries)" in {
    val cache    = seededCache("Dune")
    val fresh    = new InMemoryFreshnessStore
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", None) // fetch failed/absent
    val h        = new EnrichDetailsHandler(Map("kino-apollo" -> enricher), cache, fresh)
    val task     = taskFor("kino-apollo", cache, "Dune", enricher)

    h.handle(task) shouldBe Done
    fresh.isFresh(task.dedupKey, FreshnessKind.DetailEnrich) shouldBe false
    cache.get(cache.keyOf("Dune", None)).flatMap(_.data.get(KinoApollo)).flatMap(_.synopsis) shouldBe None
  }

  // Requirement: a task with exactly the same definition (enrich a specific film
  // for a specific cinema/group) must be rejected as a duplicate.
  "the queue" should "reject a duplicate EnrichDetails task for the same (group, film)" in {
    val cache    = seededCache("Dune")
    val queue    = new InMemoryTaskQueue
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo", Some(FilmDetail()))
    val key      = cache.keyOf("Dune", None)
    val dk       = EnrichDetailsTasks.dedupKey("kino-apollo", key)
    queue.enqueue(TaskType.EnrichDetails, dk, EnrichDetailsTasks.payload(enricher, key, "http://ref")) shouldBe EnqueueResult.Added
    queue.enqueue(TaskType.EnrichDetails, dk, EnrichDetailsTasks.payload(enricher, key, "http://ref")) shouldBe EnqueueResult.Duplicate
  }
}
