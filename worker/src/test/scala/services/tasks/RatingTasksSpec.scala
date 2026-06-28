package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.CacheKey

import java.time.{Clock, Instant, ZoneOffset}
import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.hashing.MurmurHash3

class RatingTasksSpec extends AnyFlatSpec with Matchers {

  // ── RatingHandler ─────────────────────────────────────────────────────────

  private val dueWindow = new DueWindow(4.hours)
  private def cadence    = new services.cadence.InMemoryRatingCadenceStore

  private def ratingTask(dedup: String, title: String, year: Option[Int]) =
    Task("id", TaskType.ImdbRating, dedup,
      Map(RatingTasks.TitleKey -> title, RatingTasks.YearKey -> year.map(_.toString).getOrElse("")), attempts = 1)

  "RatingHandler" should "refresh and mark fresh when stale" in {
    var calls = List.empty[(String, Option[Int])]
    val fresh = new InMemoryFreshnessStore
    val h     = new RatingHandler(TaskType.ImdbRating, FreshnessKind.ImdbRating, fresh, dueWindow, cadence, (t, y) => { calls ::= (t, y); Some("7.5") })
    h.handle(ratingTask("imdb|dune|2024", "dune", Some(2024))) shouldBe HandlerOutcome.Done
    calls shouldBe List(("dune", Some(2024)))
    fresh.isFresh("imdb|dune|2024", FreshnessKind.ImdbRating) shouldBe true
  }

  it should "skip without refreshing when already fresh" in {
    var calls = 0
    val fresh = new InMemoryFreshnessStore
    fresh.markFresh("imdb|dune|", FreshnessKind.ImdbRating)
    val h = new RatingHandler(TaskType.ImdbRating, FreshnessKind.ImdbRating, fresh, dueWindow, cadence, (_, _) => { calls += 1; Some("7.5") })
    h.handle(ratingTask("imdb|dune|", "dune", None)) shouldBe HandlerOutcome.Skipped
    calls shouldBe 0
  }

  it should "record the displayed-value change into the cadence: no-change grows the streak, a change resets it" in {
    val cad   = new services.cadence.InMemoryRatingCadenceStore
    val fresh = new InMemoryFreshnessStore
    // First refresh reports NO visible change → streak grows to 1.
    new RatingHandler(TaskType.ImdbRating, FreshnessKind.ImdbRating, fresh, new DueWindow(4.hours), cad, (_, _) => None)
      .handle(ratingTask("imdb|tmdb:7", "X", None)) shouldBe HandlerOutcome.Done
    cad.statsFor("imdb|tmdb:7").map(_.unchangedStreak) shouldBe Some(1)

    // A later refresh that DOES move the value snaps the streak back to 0 and
    // records the new displayed value as the last change.
    val later = Clock.fixed(Instant.now().plusSeconds(7.hours.toSeconds), ZoneOffset.UTC)
    new RatingHandler(TaskType.ImdbRating, FreshnessKind.ImdbRating, fresh, new DueWindow(1.hour), cad, (_, _) => Some("7.2"), later)
      .handle(ratingTask("imdb|tmdb:7", "X", None)) shouldBe HandlerOutcome.Done
    cad.statsFor("imdb|tmdb:7").map(_.unchangedStreak)        shouldBe Some(0)
    cad.statsFor("imdb|tmdb:7").flatMap(_.lastChange).map(_.to) shouldBe Some("7.2")
  }

  it should "not log a phantom change when a re-keyed row re-reports the same displayed value" in {
    // Reproduces the "Basia" flap: a title-rule fold re-keys the cache row, so the
    // per-row refresh re-resolves None → 7.3 and re-reports the SAME 7.3 twice. The
    // tmdbId-keyed cadence must see one change, then a no-change (streak grows) —
    // not two changes that pin the film to the base interval.
    val cad   = new services.cadence.InMemoryRatingCadenceStore
    val fresh = new InMemoryFreshnessStore
    new RatingHandler(TaskType.ImdbRating, FreshnessKind.ImdbRating, fresh, new DueWindow(4.hours), cad, (_, _) => Some("7.3"))
      .handle(ratingTask("imdb|tmdb:7", "X", None)) shouldBe HandlerOutcome.Done
    cad.statsFor("imdb|tmdb:7").map(_.windowChanges) shouldBe Some(1)

    val later = Clock.fixed(Instant.now().plusSeconds(7.hours.toSeconds), ZoneOffset.UTC)
    new RatingHandler(TaskType.ImdbRating, FreshnessKind.ImdbRating, fresh, new DueWindow(1.hour), cad, (_, _) => Some("7.3"), later)
      .handle(ratingTask("imdb|tmdb:7", "X", None)) shouldBe HandlerOutcome.Done
    cad.statsFor("imdb|tmdb:7").map(_.windowChanges)   shouldBe Some(1)  // still one change, no phantom
    cad.statsFor("imdb|tmdb:7").map(_.unchangedStreak) shouldBe Some(1)  // backed off instead
    cad.statsFor("imdb|tmdb:7").flatMap(_.prevChange)  shouldBe None
  }

  it should "not touch the cadence for a task it skips as still-fresh" in {
    val cad   = new services.cadence.InMemoryRatingCadenceStore
    val fresh = new InMemoryFreshnessStore
    fresh.markFresh("imdb|tmdb:7", FreshnessKind.ImdbRating)
    new RatingHandler(TaskType.ImdbRating, FreshnessKind.ImdbRating, fresh, dueWindow, cad, (_, _) => Some("7.5"))
      .handle(ratingTask("imdb|tmdb:7", "X", None)) shouldBe HandlerOutcome.Skipped
    cad.statsFor("imdb|tmdb:7") shouldBe None
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
    val h = new RatingHandler(TaskType.ImdbRating, FreshnessKind.ImdbRating, fresh, dueWindow, cadence,
      (_, _) => { calls += 1; Some("7.5") }, Clock.fixed(now, ZoneOffset.UTC))
    h.handle(ratingTask(dedup, "dune", Some(2024))) shouldBe HandlerOutcome.Done
    calls shouldBe 1
  }

  // ── first-rating-attempt delay metric ─────────────────────────────────────

  private def recordingMetrics(): (RatingLatencyMetrics, mutable.ListBuffer[(String, Double)]) = {
    val seen = mutable.ListBuffer.empty[(String, Double)]
    (new RatingLatencyMetrics { def recordFirstRatingDelay(site: String, seconds: Double): Unit = seen += ((site, seconds)) }, seen)
  }

  "RatingHandler" should "record the TMDB-resolved → first-attempt delay per site, once (not on a later attempt)" in {
    val resolvedAt = Instant.parse("2026-06-21T10:00:00Z")
    val firstAt    = resolvedAt.plusSeconds(300) // a representative post-resolution delay
    val fresh      = new InMemoryFreshnessStore
    fresh.markFresh(RatingTasks.tmdbResolvedAtKey(1454157), FreshnessKind.TmdbResolve, resolvedAt)

    val (metrics, seen) = recordingMetrics()
    val dedup = RatingTasks.dedupKey(FreshnessKind.ImdbRating, CacheKey("Kumotry", Some(2026)), Some(1454157)) // imdb|tmdb:1454157
    val h = new RatingHandler(TaskType.ImdbRating, FreshnessKind.ImdbRating, fresh, dueWindow, cadence,
      (_, _) => Some("7.5"), Clock.fixed(firstAt, ZoneOffset.UTC), metrics = metrics)

    h.handle(ratingTask(dedup, "Kumotry", Some(2026))) shouldBe HandlerOutcome.Done
    seen.toList shouldBe List(("imdb", 300.0))

    // Same tick → now fresh, so a second pickup is skipped and records nothing more.
    h.handle(ratingTask(dedup, "Kumotry", Some(2026))) shouldBe HandlerOutcome.Skipped
    seen.toList shouldBe List(("imdb", 300.0))
  }

  it should "not record a delay for a legacy title-keyed rating (no tmdbId to correlate)" in {
    val fresh = new InMemoryFreshnessStore
    fresh.markFresh(RatingTasks.tmdbResolvedAtKey(1454157), FreshnessKind.TmdbResolve, Instant.parse("2026-06-21T10:00:00Z"))
    val (metrics, seen) = recordingMetrics()
    val h = new RatingHandler(TaskType.ImdbRating, FreshnessKind.ImdbRating, fresh, dueWindow, cadence, (_, _) => Some("7.5"), metrics = metrics)

    h.handle(ratingTask("imdb|kumotry|2026", "Kumotry", Some(2026))) shouldBe HandlerOutcome.Done
    seen shouldBe empty
  }

}
