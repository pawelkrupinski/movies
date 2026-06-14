package services.staging

import models.{Cinema, Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{DetailEnricher, FilmDetail}
import services.events.{DomainEvent, InProcessEventBus, StagingFilmEnriched, TaskFinished}
import services.freshness.InMemoryFreshnessStore
import services.movies.{CacheKey, InMemoryMovieRepository}
import services.tasks.{HandlerOutcome, InMemoryTaskQueue, TaskHandler}

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._

/**
 * End-to-end: a newcomer in `pending_movies` is driven through the durable queue
 * — StagingDetail → StagingResolveTmdb → StagingResolveImdbId → StagingFold — by
 * the real handlers + `StagingReaper`, and lands in `movies`. The pump stands in
 * for the prod `TaskWorker`: claim → handle → complete → announce (`onTaskFinished`),
 * which enqueues the next step for the same loop to pick up. The fold is wired the
 * production way: the handler publishes `StagingFilmEnriched`, the folder subscribes.
 */
class StagingQueueEndToEndSpec extends AnyFlatSpec with Matchers {

  private class FakeEnricher(val cinema: Cinema, detail: Option[FilmDetail]) extends DetailEnricher {
    def detailGroup = "fake"
    def fetchFilmDetail(ref: String): Option[FilmDetail] = detail
  }

  "the queue-driven staging chain" should "incubate a newcomer all the way into movies" in {
    val staging   = new InMemoryStagingRepository
    val movies    = new InMemoryMovieRepository
    val queue     = new InMemoryTaskQueue
    val bus       = new InProcessEventBus
    val folder    = new InMemoryStagingFolder(staging, movies)
    // Wired the production way (see WorkerWiring): the fold returns the brand-new
    // films it introduced, and the subscriber schedules their ratings. Here we
    // capture the keys to prove the newcomer graduates as a promotion.
    val promoted  = ListBuffer.empty[CacheKey]
    bus.subscribe { case StagingFilmEnriched(t) => promoted ++= folder.foldGroup(t).map(_._1) }

    // A deferred-detail cinema (Helios) scrapes the film bare with a filmUrl; its
    // detail page supplies a director hint; TMDB resolves to a tmdbId but ships no
    // imdb cross-reference, so the imdb step must recover it.
    staging.upsert(Helios, "Newcomer", Some(2026),
      MovieRecord(data = Map[Source, SourceData](Helios -> SourceData(title = Some("Newcomer"), filmUrl = Some("u")))))
    val steps = new StagingSteps(staging, Seq(new FakeEnricher(Helios, Some(FilmDetail(director = Seq("Jane Doe"))))),
      resolveStaging = (_, _, r) => Some(r.copy(tmdbId = Some(1275779))),    // hit, imdb empty
      recoverImdbId  = (_, _) => Some("tt1275779"),
      freshness      = new InMemoryFreshnessStore)
    val reaper = new StagingReaper(steps, queue, staging)

    val handlers: Map[services.tasks.TaskType, TaskHandler] = Seq[TaskHandler](
      new StagingDetailHandler(steps),
      new StagingResolveTmdbHandler(steps),
      new StagingResolveImdbIdHandler(steps),
      new StagingFoldHandler(t => bus.publish(StagingFilmEnriched(t)))
    ).map(h => h.taskType -> h).toMap

    // Pump: kick the chain, then drain — completing a step announces it, which
    // enqueues the next step for this same claim-loop. Bounded so a bug can't hang.
    reaper.tick()
    var guard = 0
    Iterator.continually(queue.claim("w", 5.minutes)).takeWhile(t => t.isDefined && guard < 50).flatten.foreach { task =>
      guard += 1
      handlers(task.taskType).handle(task) match {
        case HandlerOutcome.Done | HandlerOutcome.Skipped =>
          queue.complete(task.id, "w")
          reaper.onTaskFinished.applyOrElse(TaskFinished(task.taskType, task.dedupKey, task.payload), (_: DomainEvent) => ())
        case _: HandlerOutcome.Reschedule => queue.release(task.id, "w", None, None)
      }
    }

    val folded = movies.findAll()
    folded.map(_.title) shouldBe Seq("Newcomer")
    folded.head.record.tmdbId shouldBe Some(1275779)
    folded.head.record.imdbId shouldBe Some("tt1275779")
    folded.head.record.director should contain("Jane Doe")           // detail hint carried through the fold
    staging.findAll() shouldBe empty                                  // graduated out of pending_movies
    promoted shouldBe Seq(CacheKey("Newcomer", Some(2026)))           // surfaced as a promotion → ratings scheduled
  }
}
