package services.staging

import models.{Cinema, Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{DetailEnricher, FilmDetail}
import services.freshness.InMemoryFreshnessStore
import services.tasks.{HandlerOutcome, StagingTaskKeys, Task, TaskType}

/** Specs for the four thin staging handlers — each parses its payload, runs the
 *  matching `StagingSteps` step, and maps the result to a `HandlerOutcome`. */
class StagingTaskHandlersSpec extends AnyFlatSpec with Matchers {

  private class FakeEnricher(val cinema: Cinema, detail: Option[FilmDetail]) extends DetailEnricher {
    def detailGroup = "fake"
    def fetchFilmDetail(ref: String): Option[FilmDetail] = detail
  }

  private def task(taskType: TaskType, payload: Map[String, String], attempts: Int = 1) =
    Task(id = "t1", taskType = taskType, dedupKey = "k", payload = payload, attempts = attempts)

  private def listingRow(title: String): MovieRecord =
    MovieRecord(data = Map[Source, SourceData](Helios -> SourceData(title = Some(title), filmUrl = Some("u"))))

  private def steps(repository: InMemoryStagingRepository, enrichers: Seq[DetailEnricher],
                    resolve: (String, Option[Int], MovieRecord) => Option[MovieRecord],
                    recover: (String, Option[Int]) => Option[String] = (_, _) => None) =
    new StagingSteps(repository, enrichers, resolve, recover, new InMemoryFreshnessStore)

  "StagingDetailHandler" should "fetch the cinema's detail and report Done" in {
    val repository = new InMemoryStagingRepository
    repository.upsert(Helios, "Film", Some(2026), listingRow("Film"))
    val handler = new StagingDetailHandler(steps(repository, Seq(new FakeEnricher(Helios, Some(FilmDetail(synopsis = Some("p"))))), (_, _, r) => Some(r)))

    handler.handle(task(TaskType.StagingDetail, StagingTaskKeys.detailPayload("Film", Helios.displayName))) shouldBe HandlerOutcome.Done
    repository.findAll().head.record.data(Helios).synopsis shouldBe Some("p")
  }

  it should "reschedule when the deferred detail fetch hasn't landed" in {
    val repository = new InMemoryStagingRepository
    repository.upsert(Helios, "Film", Some(2026), listingRow("Film"))
    val handler = new StagingDetailHandler(steps(repository, Seq(new FakeEnricher(Helios, None)), (_, _, r) => Some(r)))

    handler.handle(task(TaskType.StagingDetail, StagingTaskKeys.detailPayload("Film", Helios.displayName))) shouldBe a[HandlerOutcome.Reschedule]
  }

  it should "give up and report Done once the retry budget is exhausted, marking detail ready so the film graduates" in {
    val repository = new InMemoryStagingRepository
    repository.upsert(Helios, "Film", Some(2026), listingRow("Film"))
    val s = steps(repository, Seq(new FakeEnricher(Helios, None)), (_, _, r) => Some(r))
    val handler = new StagingDetailHandler(s)
    val payload = StagingTaskKeys.detailPayload("Film", Helios.displayName)

    handler.handle(task(TaskType.StagingDetail, payload)) shouldBe a[HandlerOutcome.Reschedule]  // early attempt — retry
    handler.handle(task(TaskType.StagingDetail, payload, attempts = StagingDetailHandler.MaxDetailAttempts)) shouldBe HandlerOutcome.Done
    s.detailReady(repository.findAll().head) shouldBe true
  }

  it should "skip an orphaned task for an unknown cinema" in {
    val repository = new InMemoryStagingRepository
    val handler = new StagingDetailHandler(steps(repository, Seq.empty, (_, _, r) => Some(r)))
    handler.handle(task(TaskType.StagingDetail, StagingTaskKeys.detailPayload("Film", "No Such Cinema"))) shouldBe HandlerOutcome.Skipped
  }

  // Helios has no enricher here, so the detail gate is satisfied and these specs
  // exercise the resolve outcome itself (detail readiness is covered elsewhere).
  "StagingResolveTmdbHandler" should "report Done on a hit and stamp the tmdbId" in {
    val repository = new InMemoryStagingRepository
    repository.upsert(Helios, "Film", Some(2026), listingRow("Film"))
    val handler = new StagingResolveTmdbHandler(steps(repository, Seq.empty, (_, _, r) => Some(r.copy(tmdbId = Some(5)))))

    handler.handle(task(TaskType.StagingResolveTmdb, StagingTaskKeys.titlePayload("Film"))) shouldBe HandlerOutcome.Done
    repository.findAll().head.record.tmdbId shouldBe Some(5)
  }

  it should "reschedule on a transient TMDB miss" in {
    val repository = new InMemoryStagingRepository
    repository.upsert(Helios, "Film", Some(2026), listingRow("Film"))
    val handler = new StagingResolveTmdbHandler(steps(repository, Seq.empty, (_, _, _) => None))

    handler.handle(task(TaskType.StagingResolveTmdb, StagingTaskKeys.titlePayload("Film"))) shouldBe a[HandlerOutcome.Reschedule]
  }

  "StagingResolveImdbIdHandler" should "recover + stamp the imdbId and report Done" in {
    val repository = new InMemoryStagingRepository
    repository.upsert(Helios, "Film", Some(2026), listingRow("Film").copy(tmdbId = Some(5)))   // resolved, no imdb
    val handler = new StagingResolveImdbIdHandler(steps(repository, Seq.empty, (_, _, r) => Some(r), (_, _) => Some("tt5")))

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
