package services.staging

import models.{Cinema, CinemaShowing, Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.freshness.InMemoryFreshnessStore
import services.resolution.TmdbAttempt
import services.tasks.{HandlerOutcome, StagingTaskKeys, Task, TaskType}
import services.cinemas.common.{DetailEnricher, FilmDetail}
import services.movies.SingleCountryNormalizer.titleNormalizer

/** Specs for the four thin staging handlers — each parses its payload, runs the
 *  matching `StagingSteps` step, and maps the result to a `HandlerOutcome`. */
class StagingTaskHandlersSpec extends AnyFlatSpec with Matchers {

  private class FakeEnricher(val cinema: Cinema, detail: Option[FilmDetail]) extends DetailEnricher {
    def detailGroup = "fake"
    def fetchFilmDetail(ref: String): Option[FilmDetail] = detail
  }

  private def task(taskType: TaskType, payload: Map[String, String], attempts: Int = 1) =
    Task(id = "t1", taskType = taskType, dedupKey = "k", payload = payload, attempts = attempts)

  // Slot keyed per shown title (`CinemaShowing`), as the scrape-divert path writes
  // it — so the staging detail step merges into it.
  private def listingRow(title: String): MovieRecord =
    MovieRecord(data = Map[Source, SourceData](CinemaShowing.keyFor(Helios, title, titleNormalizer) -> SourceData(title = Some(title), filmUrl = Some("u"))))

  private def steps(repository: InMemoryStagingRepository, enrichers: Seq[DetailEnricher],
                    resolve: (String, Option[Int], MovieRecord) => Option[MovieRecord],
                    recover: (String, Option[Int], models.MovieRecord) => Option[String] = (_, _, _) => None,
                    clock: java.time.Clock = java.time.Clock.systemUTC()) =
    new StagingSteps(repository, enrichers, resolve, recover, new InMemoryFreshnessStore, clock = clock)

  "StagingDetailHandler" should "fetch the cinema's detail and report Done" in {
    val repository = new InMemoryStagingRepository(normalizer = titleNormalizer)
    repository.upsert(Helios, "Film", Some(2026), listingRow("Film"))
    val handler = new StagingDetailHandler(steps(repository, Seq(new FakeEnricher(Helios, Some(FilmDetail(synopsis = Some("p"))))), (_, _, r) => Some(r)))

    handler.handle(task(TaskType.StagingDetail, StagingTaskKeys.detailPayload("Film", Helios.displayName, titleNormalizer))) shouldBe HandlerOutcome.Done
    repository.findAll().head.record.cinemaData(Helios).synopsis shouldBe Some("p")
  }

  it should "reschedule when the deferred detail fetch hasn't landed" in {
    val repository = new InMemoryStagingRepository(normalizer = titleNormalizer)
    repository.upsert(Helios, "Film", Some(2026), listingRow("Film"))
    val handler = new StagingDetailHandler(steps(repository, Seq(new FakeEnricher(Helios, None)), (_, _, r) => Some(r)))

    handler.handle(task(TaskType.StagingDetail, StagingTaskKeys.detailPayload("Film", Helios.displayName, titleNormalizer))) shouldBe a[HandlerOutcome.Reschedule]
  }

  it should "give up and report Done once the retry budget is exhausted, marking detail ready so the film graduates" in {
    val repository = new InMemoryStagingRepository(normalizer = titleNormalizer)
    repository.upsert(Helios, "Film", Some(2026), listingRow("Film"))
    val s = steps(repository, Seq(new FakeEnricher(Helios, None)), (_, _, r) => Some(r))
    val handler = new StagingDetailHandler(s)
    val payload = StagingTaskKeys.detailPayload("Film", Helios.displayName, titleNormalizer)

    handler.handle(task(TaskType.StagingDetail, payload)) shouldBe a[HandlerOutcome.Reschedule]  // early attempt — retry
    handler.handle(task(TaskType.StagingDetail, payload, attempts = StagingDetailHandler.MaxDetailAttempts)) shouldBe HandlerOutcome.Done
    s.detailReady(repository.findAll().head) shouldBe true
  }

  it should "skip an orphaned task for an unknown cinema" in {
    val repository = new InMemoryStagingRepository(normalizer = titleNormalizer)
    val handler = new StagingDetailHandler(steps(repository, Seq.empty, (_, _, r) => Some(r)))
    handler.handle(task(TaskType.StagingDetail, StagingTaskKeys.detailPayload("Film", "No Such Cinema", titleNormalizer))) shouldBe HandlerOutcome.Skipped
  }

  // Helios has no enricher here, so the detail gate is satisfied and these specs
  // exercise the resolve outcome itself (detail readiness is covered elsewhere).
  "StagingResolveTmdbHandler" should "report Done on a hit and stamp the tmdbId" in {
    val repository = new InMemoryStagingRepository(normalizer = titleNormalizer)
    repository.upsert(Helios, "Film", Some(2026), listingRow("Film"))
    val handler = new StagingResolveTmdbHandler(steps(repository, Seq.empty, (_, _, r) => Some(r.copy(tmdbId = Some(5)))))

    handler.handle(task(TaskType.StagingResolveTmdb, StagingTaskKeys.titlePayload("Film"))) shouldBe HandlerOutcome.Done
    repository.findAll().head.record.tmdbId shouldBe Some(5)
  }

  it should "reschedule on a transient TMDB miss" in {
    val repository = new InMemoryStagingRepository(normalizer = titleNormalizer)
    repository.upsert(Helios, "Film", Some(2026), listingRow("Film"))
    val handler = new StagingResolveTmdbHandler(steps(repository, Seq.empty, (_, _, _) => None))

    handler.handle(task(TaskType.StagingResolveTmdb, StagingTaskKeys.titlePayload("Film"))) shouldBe a[HandlerOutcome.Reschedule]
  }

  it should "complete (not reschedule) while a cinema still owes its detail, so the chain re-enqueues instead of backing off" in {
    // The UK stall of 2026-07-27: six films sat at `staging detail not ready`,
    // attempts 9-10, `nextEligibleAt` 13-30 min out — long after the detail they
    // were waiting for had landed. Rescheduling parks the task under the same
    // exponential backoff a FAILURE gets, and `TaskQueue.enqueue` is insert-only,
    // so nothing can pull a waiting task's backoff forward. Completing hands the
    // film back to `StagingReaper`, which re-enqueues a fresh task (attempts=0)
    // the moment every venue's detail is in — see the reaper's "enqueue
    // StagingResolveTmdb once the detail is present".
    val repository = new InMemoryStagingRepository(normalizer = titleNormalizer)
    repository.upsert(Helios, "Film", Some(2026), listingRow("Film"))
    var resolveCalled = false
    val handler = new StagingResolveTmdbHandler(steps(repository,
      Seq(new FakeEnricher(Helios, Some(FilmDetail(synopsis = Some("p"))))),   // enricher wired, detail step not run yet
      (_, _, r) => { resolveCalled = true; Some(r.copy(tmdbId = Some(5))) }))

    handler.handle(task(TaskType.StagingResolveTmdb, StagingTaskKeys.titlePayload("Film"))) shouldBe HandlerOutcome.Skipped
    resolveCalled shouldBe false                                       // gate held — no half-informed resolve
    repository.findAll().head.record.tmdbConcluded shouldBe false      // still owed, so the reaper re-enqueues it
  }

  // `resolveStaging` answers None only for a TRANSIENT failure — a definitive TMDB miss comes back
  // concluded (`MovieService.resolveStagingRecord`). A six-claim give-up budget (~2.5 min) once
  // turned every TMDB blip into a film concluded no-match; retrying for ever instead keeps a film
  // whose lookup fails for days in staging, invisible. Between the two: a failure is retried until
  // it has lasted `TransientResolveCeiling` (timed from its FIRST failure, across task attempts,
  // restarts and re-enqueues), then folded as an unanswered no-match its next resolve re-tries.
  it should "retry a transient failure shorter than the ceiling, however many attempts it takes" in {
    val repository = new InMemoryStagingRepository(normalizer = titleNormalizer)
    repository.upsert(Helios, "Throwy Film", Some(2026), listingRow("Throwy Film"))
    val clock   = new tools.MutableClock(java.time.Instant.parse("2026-09-24T12:00:00Z"))
    var answer  = Option.empty[MovieRecord]
    val handler = new StagingResolveTmdbHandler(steps(repository, Seq.empty, (_, _, r) => answer.map(_ => r.copy(tmdbId = Some(9))), clock = clock))
    val payload = StagingTaskKeys.titlePayload("Throwy Film")

    Seq(1, 6, 12, 50).foreach { attempts =>
      handler.handle(task(TaskType.StagingResolveTmdb, payload, attempts = attempts)) shouldBe a[HandlerOutcome.Reschedule]
      clock.advanceSeconds(StagingSteps.TransientResolveCeiling.toSeconds / 5)
    }
    repository.findAll().head.record.tmdbConcluded shouldBe false
    // TMDB answers inside the ceiling: resolved on its answer.
    answer = Some(MovieRecord())
    handler.handle(task(TaskType.StagingResolveTmdb, payload)) shouldBe HandlerOutcome.Done
    repository.findAll().head.record.tmdbId shouldBe Some(9)
  }

  it should "fold a film as an unanswered no-match once its resolve has failed for longer than the ceiling" in {
    val repository = new InMemoryStagingRepository(normalizer = titleNormalizer)
    repository.upsert(Helios, "Throwy Film", Some(2026), listingRow("Throwy Film"))
    val clock   = new tools.MutableClock(java.time.Instant.parse("2026-09-24T12:00:00Z"))
    val handler = new StagingResolveTmdbHandler(steps(repository, Seq.empty, (_, _, _) => None, clock = clock))
    val payload = StagingTaskKeys.titlePayload("Throwy Film")

    handler.handle(task(TaskType.StagingResolveTmdb, payload)) shouldBe a[HandlerOutcome.Reschedule]
    clock.advanceSeconds(StagingSteps.TransientResolveCeiling.toSeconds + 1)
    handler.handle(task(TaskType.StagingResolveTmdb, payload)) shouldBe HandlerOutcome.Done
    val row = repository.findAll().head
    row.record.tmdbNoMatch shouldBe true
    // Unanswered, not a miss: it covers no evidence, so the row's next resolve searches again.
    val attempt = row.record.tmdbAttempt.getOrElse(fail("no attempt stamped"))
    attempt.covers(TmdbAttempt.on(row.record.evidence, row.record.resolverOriginalTitles, clock.instant()).evidence, clock.instant()) shouldBe false
  }

  "StagingResolveImdbIdHandler" should "recover + stamp the imdbId and report Done" in {
    val repository = new InMemoryStagingRepository(normalizer = titleNormalizer)
    repository.upsert(Helios, "Film", Some(2026), listingRow("Film").copy(tmdbId = Some(5)))   // resolved, no imdb
    val handler = new StagingResolveImdbIdHandler(steps(repository, Seq.empty, (_, _, r) => Some(r), (_, _, _) => Some("tt5")))

    handler.handle(task(TaskType.StagingResolveImdbId, StagingTaskKeys.titlePayload("Film"))) shouldBe HandlerOutcome.Done
    repository.findAll().head.record.imdbId shouldBe Some("tt5")
  }

  "StagingFoldHandler" should "invoke the group fold with the payload's title" in {
    var folded = Option.empty[String]
    val handler = new StagingFoldHandler(t => folded = Some(t))
    handler.handle(task(TaskType.StagingFold, StagingTaskKeys.titlePayload("Film"))) shouldBe HandlerOutcome.Done
    folded shouldBe Some("Film")
  }
}
