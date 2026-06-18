package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{ImdbIdMissing, ImdbIdResolved, TmdbResolved}
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import services.events.InProcessEventBus

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

  private def newCache() = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus())

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
}
