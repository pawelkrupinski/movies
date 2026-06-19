package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{ImdbIdMissing, ImdbIdResolved, TmdbResolved}
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import services.events.InProcessEventBus

import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.duration._
import scala.util.hashing.MurmurHash3

class RatingTasksSpec extends AnyFlatSpec with Matchers {

  // ── RatingHandler ─────────────────────────────────────────────────────────

  private val dueWindow = new DueWindow(4.hours)

  private def ratingTask(dedup: String, title: String, year: Option[Int]) =
    Task("id", TaskType.ImdbRating, dedup,
      Map(RatingTasks.TitleKey -> title, RatingTasks.YearKey -> year.map(_.toString).getOrElse("")), attempts = 1)

  "RatingHandler" should "refresh and mark fresh when stale" in {
    var calls = List.empty[(String, Option[Int])]
    val fresh = new InMemoryFreshnessStore
    val h     = new RatingHandler(TaskType.ImdbRating, FreshnessKind.ImdbRating, fresh, dueWindow, (t, y) => calls ::= (t, y))
    h.handle(ratingTask("imdb|dune|2024", "dune", Some(2024))) shouldBe HandlerOutcome.Done
    calls shouldBe List(("dune", Some(2024)))
    fresh.isFresh("imdb|dune|2024", FreshnessKind.ImdbRating) shouldBe true
  }

  it should "skip without refreshing when already fresh" in {
    var calls = 0
    val fresh = new InMemoryFreshnessStore
    fresh.markFresh("imdb|dune|", FreshnessKind.ImdbRating)
    val h = new RatingHandler(TaskType.ImdbRating, FreshnessKind.ImdbRating, fresh, dueWindow, (_, _) => calls += 1)
    h.handle(ratingTask("imdb|dune|", "dune", None)) shouldBe HandlerOutcome.Skipped
    calls shouldBe 0
  }

  it should "act on (not skip) a task the reaper deems due even when it is still inside the rolling TTL" in {
    // Regression: the reaper enqueues on a phase-window boundary; the handler used
    // to re-gate on a rolling 4h TTL. A row refreshed just before its boundary is
    // due again just after it, yet still within the TTL — so the handler skipped a
    // task the reaper kept enqueuing every tick, churning the queue without ever
    // refreshing it. Both now share DueWindow, so a due task is always acted on.
    val period = 4.hours.toMillis
    val dedup  = "imdb|dune|2024"
    val phase  = Math.floorMod(MurmurHash3.stringHash(dedup).toLong, period)
    val w      = 100L
    val stampedAt = Instant.ofEpochMilli(phase + w * period + period - 60000)       // 1 min before the boundary (window w)
    val now       = Instant.ofEpochMilli(phase + (w + 1) * period + 60000)          // 1 min after  the boundary (window w+1)
    // Elapsed is only ~2 min, so the old rolling-TTL re-gate (4h) would skip it.
    var calls = 0
    val fresh = new InMemoryFreshnessStore
    fresh.markFresh(dedup, FreshnessKind.ImdbRating, stampedAt)
    val h = new RatingHandler(TaskType.ImdbRating, FreshnessKind.ImdbRating, fresh, dueWindow,
      (_, _) => calls += 1, Clock.fixed(now, ZoneOffset.UTC))
    h.handle(ratingTask(dedup, "dune", Some(2024))) shouldBe HandlerOutcome.Done
    calls shouldBe 1
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
