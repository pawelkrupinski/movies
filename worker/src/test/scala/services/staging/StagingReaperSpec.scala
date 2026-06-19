package services.staging

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.FakeDetailEnricher
import services.events.TaskFinished
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.tasks.{InMemoryTaskQueue, StagingTaskKeys, TaskType}

/** Specs for the staging state machine: given a film's `pending_movies` state,
 *  `StagingReaper` enqueues exactly the one next step it needs, idempotently, and
 *  advances the chain off `TaskFinished`. */
class StagingReaperSpec extends AnyFlatSpec with Matchers {

  private val enricher = new FakeDetailEnricher(Helios, "fake")        // deferring detail cinema

  /** Wire a reaper over a fresh queue + repository seeded with `rows`. The
   *  StagingSteps' resolve/recover stubs are never reached (the reaper only reads
   *  repository state + readiness), so they're inert. The freshness store is
   *  returned so a test can mark a film's detail "done" (the readiness signal). */
  private def fixture(rows: (String, Option[Int], MovieRecord)*) = {
    val repository = new InMemoryStagingRepository
    rows.foreach { case (t, y, r) => repository.upsert(Helios, t, y, r) }
    val freshness = new InMemoryFreshnessStore
    val steps     = new StagingSteps(repository, Seq(enricher), (_, _, _) => None, (_, _) => None, freshness)
    val queue     = new InMemoryTaskQueue
    val reaper    = new StagingReaper(steps, queue, repository)
    (queue, reaper, repository, freshness)
  }

  /** Mark a film's deferred-detail fetch as done — the signal `detailReady` reads. */
  private def markDetailDone(freshness: InMemoryFreshnessStore, title: String): Unit =
    freshness.markFresh(StagingTaskKeys.detailDedup(title, Helios.displayName), FreshnessKind.DetailEnrich)

  /** Mark IMDb recovery as already attempted (the best-effort, one-shot signal). */
  private def markImdbAttempted(freshness: InMemoryFreshnessStore, title: String): Unit =
    freshness.markFresh(StagingTaskKeys.resolveImdbDedup(title), FreshnessKind.ImdbRating)

  private def listing(title: String, year: Option[Int]): MovieRecord =
    MovieRecord(data = Map[Source, SourceData](Helios -> SourceData(title = Some(title), filmUrl = Some("u"))))

  private def active(queue: InMemoryTaskQueue): Seq[(String, String)] =
    queue.monitor().active.map(t => (t.taskType, t.dedupKey))

  "tick" should "enqueue a StagingDetail task for a deferred cinema whose detail hasn't landed" in {
    val (queue, reaper, _, _) = fixture(("Newcomer", Some(2026), listing("Newcomer", Some(2026))))
    reaper.tick() shouldBe 1
    active(queue) shouldBe Seq((TaskType.StagingDetail.name, StagingTaskKeys.detailDedup("Newcomer", Helios.displayName)))
  }

  it should "enqueue StagingResolveTmdb once the detail is present" in {
    val (queue, reaper, _, freshness) = fixture(("Ready", Some(2026), listing("Ready", Some(2026))))
    markDetailDone(freshness, "Ready")
    reaper.tick() shouldBe 1
    active(queue) shouldBe Seq((TaskType.StagingResolveTmdb.name, StagingTaskKeys.resolveTmdbDedup("Ready")))
  }

  it should "enqueue StagingResolveImdbId for a resolved row missing its imdbId" in {
    val resolved = listing("Pucio", Some(2026)).copy(tmdbId = Some(1645035))   // hit, no imdbId
    val (queue, reaper, _, _) = fixture(("Pucio", Some(2026), resolved))
    reaper.tick() shouldBe 1
    active(queue) shouldBe Seq((TaskType.StagingResolveImdbId.name, StagingTaskKeys.resolveImdbDedup("Pucio")))
  }

  it should "fold (NOT re-enqueue imdb) once IMDb recovery was attempted without a match" in {
    val resolved = listing("Pucio", Some(2026)).copy(tmdbId = Some(1645035))   // tmdbId, still no imdbId
    val (queue, reaper, _, freshness) = fixture(("Pucio", Some(2026), resolved))
    markImdbAttempted(freshness, "Pucio")                                       // recovery ran, found nothing
    reaper.tick() shouldBe 1
    active(queue).map(_._1) shouldBe Seq(TaskType.StagingFold.name)             // folds, doesn't loop
  }

  it should "enqueue StagingFold for a concluded row that has its imdbId" in {
    val concluded = listing("Done", Some(2026)).copy(tmdbId = Some(7), imdbId = Some("tt7"))
    val (queue, reaper, _, _) = fixture(("Done", Some(2026), concluded))
    reaper.tick() shouldBe 1
    active(queue) shouldBe Seq((TaskType.StagingFold.name, StagingTaskKeys.foldDedup("Done")))
  }

  it should "enqueue StagingFold for a definitive tmdbNoMatch (no imdb needed)" in {
    val noMatch = listing("Obscure", Some(2026)).copy(tmdbNoMatch = true)
    val (queue, reaper, _, _) = fixture(("Obscure", Some(2026), noMatch))
    reaper.tick() shouldBe 1
    active(queue).map(_._1) shouldBe Seq(TaskType.StagingFold.name)
  }

  it should "fold the whole sanitize group as ONE task across year-variants" in {
    val a = listing("Multi", Some(2025)).copy(tmdbId = Some(9), imdbId = Some("tt9"))
    val b = listing("Multi", Some(2026)).copy(tmdbId = Some(9), imdbId = Some("tt9"))
    val (queue, reaper, _, _) = fixture(("Multi", Some(2025), a), ("Multi", Some(2026), b))
    reaper.tick() shouldBe 1                                          // group-scoped fold, not per-year
    active(queue) shouldBe Seq((TaskType.StagingFold.name, StagingTaskKeys.foldDedup("Multi")))
  }

  it should "be idempotent — a second tick adds nothing while the step is still active" in {
    val (queue, reaper, _, freshness) = fixture(("Ready", Some(2026), listing("Ready", Some(2026))))
    markDetailDone(freshness, "Ready")
    reaper.tick() shouldBe 1
    reaper.tick() shouldBe 0
    active(queue) should have size 1
  }

  "onTaskFinished" should "advance a resolved+imdb film to the fold step" in {
    val concluded = listing("Done", Some(2026)).copy(tmdbId = Some(7), imdbId = Some("tt7"))
    val (queue, reaper, _, _) = fixture(("Done", Some(2026), concluded))
    reaper.onTaskFinished(TaskFinished(TaskType.StagingResolveTmdb, "k", StagingTaskKeys.titlePayload("Done")))
    active(queue).map(_._1) shouldBe Seq(TaskType.StagingFold.name)
  }

  it should "ignore a finished StagingFold (terminal) and every non-staging task" in {
    val (_, reaper, _, _) = fixture(("Done", Some(2026), listing("Done", Some(2026))))
    reaper.onTaskFinished.isDefinedAt(TaskFinished(TaskType.StagingFold, "k", StagingTaskKeys.titlePayload("Done"))) shouldBe false
    reaper.onTaskFinished.isDefinedAt(TaskFinished(TaskType.ScrapeCinema, "k", Map.empty)) shouldBe false
  }

  "stepCounts" should "tally DISTINCT films by the step each needs next (the metrics view)" in {
    val resolved  = listing("Pucio", Some(2026)).copy(tmdbId = Some(1645035))                  // hit, no imdb → resolve_imdb
    val concluded = listing("Done", Some(2026)).copy(tmdbId = Some(7), imdbId = Some("tt7"))   // fold
    val multiA    = listing("Multi", Some(2025)).copy(tmdbId = Some(9), imdbId = Some("tt9"))  // same film, two year-rows…
    val multiB    = listing("Multi", Some(2026)).copy(tmdbId = Some(9), imdbId = Some("tt9"))  // …count once
    val (_, reaper, _, freshness) = fixture(
      ("Newcomer", Some(2026), listing("Newcomer", Some(2026))),   // detail not landed → detail
      ("Ready",    Some(2026), listing("Ready", Some(2026))),      // detail done, unresolved → resolve_tmdb
      ("Pucio",    Some(2026), resolved),
      ("Done",     Some(2026), concluded),
      ("Multi",    Some(2025), multiA), ("Multi", Some(2026), multiB)
    )
    markDetailDone(freshness, "Ready")

    reaper.stepCounts() shouldBe Map(
      StagingStep.Detail      -> 1,   // Newcomer
      StagingStep.ResolveTmdb -> 1,   // Ready
      StagingStep.ResolveImdb -> 1,   // Pucio
      StagingStep.Fold        -> 2    // Done + Multi (Multi's two rows are one film)
    )
  }
}
