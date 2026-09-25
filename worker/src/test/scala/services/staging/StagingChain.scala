package services.staging

import models.{Cinema, MovieRecord, Source, SourceData}
import services.cinemas.common.{DetailEnricher, FilmDetail}
import services.events.{DomainEvent, InProcessEventBus, StagingFilmEnriched, TaskFinished}
import services.freshness.InMemoryFreshnessStore
import services.movies.{CacheKey, InMemoryMovieRepository}
import services.tasks.{HandlerOutcome, InMemoryTaskQueue, Task, TaskHandler, TaskType}

import scala.collection.mutable
import scala.concurrent.duration._

/**
 * The queue-driven staging chain wired the production way, over whichever
 * [[StagingRepository]] a spec hands it: the real handlers, the real [[StagingReaper]], and
 * a fold subscriber on the bus. [[pump]] stands in for the prod `TaskWorker` — claim →
 * handle → complete → announce (`onTaskFinished`) — so each finished step enqueues the
 * next for the same loop to pick up. No backstop tick runs unless a spec calls one.
 */
final class StagingChain(
  val staging:    StagingRepository,
  enrichers:      Seq[DetailEnricher],
  resolveStaging: (String, Option[Int], MovieRecord) => Option[MovieRecord] = (_, _, r) => Some(r.copy(tmdbId = Some(1275779))),
  recoverImdbId:  (String, Option[Int], MovieRecord) => Option[String]     = (_, _, _) => Some("tt1275779")
) {
  val movies   = new InMemoryMovieRepository(normalizer = staging.normalizer)
  val queue    = new InMemoryTaskQueue
  val bus      = new InProcessEventBus
  /** Keys the fold announced as brand-new films — production's rating enqueue. */
  val promoted = mutable.ListBuffer.empty[CacheKey]
  bus.subscribe(new FoldOnStagingEnriched(
    new InMemoryStagingFolder(staging, movies, normalizer = staging.normalizer), staging, (key, _) => promoted += key).onStagingFilmEnriched)

  val steps  = new StagingSteps(staging, enrichers, resolveStaging, recoverImdbId, new InMemoryFreshnessStore)
  val reaper = new StagingReaper(steps, queue, staging)

  private val handlers: Map[TaskType, TaskHandler] = Seq[TaskHandler](
    new StagingDetailHandler(steps),
    new StagingResolveTmdbHandler(steps),
    new StagingResolveImdbIdHandler(steps),
    new StagingFoldHandler(t => bus.publish(StagingFilmEnriched(t)))
  ).map(h => h.taskType -> h).toMap

  /** Run at most `limit` claimed tasks. A step that didn't finish (Reschedule/Deferred)
   *  is released, and the limit bounds the loop so a bug can't hang it. Returns how many
   *  tasks it ran. */
  def pump(limit: Int = 50): Int =
    Iterator.continually(queue.claim("w", 5.minutes)).takeWhile(_.isDefined).flatten.take(limit).map(run).size

  private def run(task: Task): Unit =
    handlers(task.taskType).handle(task) match {
      case HandlerOutcome.Done | HandlerOutcome.Skipped =>
        queue.complete(task.id, "w")
        reaper.onTaskFinished.applyOrElse(TaskFinished(task.taskType, task.dedupKey, task.payload), (_: DomainEvent) => ())
      case _ => queue.release(task.id, "w", None, None)
    }
}

object StagingChain {
  /** A deferred-detail enricher for one venue that counts its detail fetches. */
  final class CountingEnricher(val cinema: Cinema, detail: Option[FilmDetail] = Some(FilmDetail(director = Seq("Jane Doe"))))
      extends DetailEnricher {
    var fetches = 0
    def detailGroup = "fake"
    def fetchFilmDetail(ref: String): Option[FilmDetail] = { fetches += 1; detail }
  }

  /** A bare listing with a film URL — what a deferred-detail venue stages. */
  def listing(cinema: Source, title: String): MovieRecord =
    MovieRecord(data = Map[Source, SourceData](cinema -> SourceData(title = Some(title), filmUrl = Some("u"))))
}
