package services.tasks

import models.{CinemaMovie, KinoApollo, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{ImdbIdMissing, ImdbIdResolved, TmdbResolved}
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepo}
import services.events.InProcessEventBus

import java.time.LocalDateTime

class RatingTasksSpec extends AnyFlatSpec with Matchers {

  // ── RatingHandler ─────────────────────────────────────────────────────────

  private def ratingTask(dedup: String, title: String, year: Option[Int]) =
    Task("id", TaskType.ImdbRating, dedup,
      Map(RatingTasks.TitleKey -> title, RatingTasks.YearKey -> year.map(_.toString).getOrElse("")), attempts = 1)

  "RatingHandler" should "refresh and mark fresh when stale" in {
    var calls = List.empty[(String, Option[Int])]
    val fresh = new InMemoryFreshnessStore
    val h     = new RatingHandler(TaskType.ImdbRating, FreshnessKind.ImdbRating, fresh, (t, y) => calls ::= (t, y))
    h.handle(ratingTask("imdb|dune|2024", "dune", Some(2024))) shouldBe HandlerOutcome.Done
    calls shouldBe List(("dune", Some(2024)))
    fresh.isFresh("imdb|dune|2024", FreshnessKind.ImdbRating) shouldBe true
  }

  it should "skip without refreshing when already fresh" in {
    var calls = 0
    val fresh = new InMemoryFreshnessStore
    fresh.markFresh("imdb|dune|", FreshnessKind.ImdbRating)
    val h = new RatingHandler(TaskType.ImdbRating, FreshnessKind.ImdbRating, fresh, (_, _) => calls += 1)
    h.handle(ratingTask("imdb|dune|", "dune", None)) shouldBe HandlerOutcome.Skipped
    calls shouldBe 0
  }

  // ── RatingEnqueuer (bus subscribers enqueue) ──────────────────────────────

  private def newCache() = new CaffeineMovieCache(new InMemoryMovieRepo(), new InProcessEventBus())

  "RatingEnqueuer" should "enqueue all four rating tasks on TmdbResolved" in {
    val queue = new InMemoryTaskQueue
    new RatingEnqueuer(newCache(), queue).onTmdbResolved(TmdbResolved("Dune", Some(2024), "tt1"))
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 4L
  }

  it should "enqueue only the non-IMDb ratings on ImdbIdMissing" in {
    val queue = new InMemoryTaskQueue
    new RatingEnqueuer(newCache(), queue).onImdbIdMissing(ImdbIdMissing("Dune", Some(2024), "Dune"))
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 3L
  }

  it should "enqueue the IMDb rating on ImdbIdResolved" in {
    val queue = new InMemoryTaskQueue
    new RatingEnqueuer(newCache(), queue).onImdbIdResolved(ImdbIdResolved("Dune", Some(2024), "tt1"))
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 1L
  }

  // ── EnrichmentReaper ──────────────────────────────────────────────────────

  private def seedRow(cache: CaffeineMovieCache, title: String)(edit: models.MovieRecord => models.MovieRecord): Unit = {
    cache.recordCinemaScrape(KinoApollo, Seq(CinemaMovie(
      Movie(title), KinoApollo, None, None, None, Seq.empty, Seq.empty,
      Seq(Showtime(LocalDateTime.of(2026, 6, 8, 18, 0), Some("https://book"))))))
    cache.putIfPresent(cache.keyOf(title, None), edit)
  }

  "EnrichmentReaper" should "enqueue a task per source for an eligible row, and dedup on a second sweep" in {
    val cache  = newCache()
    val queue  = new InMemoryTaskQueue
    seedRow(cache, "Resolved")(_.copy(imdbId = Some("tt1"), tmdbId = Some(2))) // eligible for all 4 sources
    val reaper = new EnrichmentReaper(cache, queue, new InMemoryFreshnessStore)
    reaper.sweepOnce() shouldBe 4
    reaper.sweepOnce() shouldBe 0 // tasks still active → deduped
    queue.countByState().getOrElse(TaskState.Waiting, 0L) shouldBe 4L
  }

  it should "enqueue only the non-IMDb ratings for a row resolved on TMDB but without an imdbId" in {
    val cache = newCache()
    val queue = new InMemoryTaskQueue
    seedRow(cache, "TmdbOnly")(_.copy(tmdbId = Some(2)))
    new EnrichmentReaper(cache, queue, new InMemoryFreshnessStore).sweepOnce() shouldBe 3
  }

  it should "enqueue nothing for an unresolved row" in {
    val cache = newCache()
    val queue = new InMemoryTaskQueue
    seedRow(cache, "Bare")(identity)
    new EnrichmentReaper(cache, queue, new InMemoryFreshnessStore).sweepOnce() shouldBe 0
  }
}
