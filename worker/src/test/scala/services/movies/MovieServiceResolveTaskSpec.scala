package services.movies

import clients.TmdbClient
import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{InProcessEventBus, MovieDetailsComplete}
import services.tasks.{EnrichTaskKeys, InMemoryTaskQueue, TaskType}
import tools.RoutingHttpFetch

import java.time.Instant
import scala.concurrent.duration._

/**
 * Single-movie TMDB resolution is a real queued `ResolveTmdb` task in the
 * normal enrichment flow — not just an inline side-effect of the bus event.
 * That's what gives the `/debug` "tmdb-unresolved" section a live queue place
 * (it matches rows against `ResolveTmdb` tasks by dedup key), and what makes a
 * resolution retryable + deduped through the same machinery as everything else.
 *
 * The resolution WORK still runs through the shared `resolveTmdbOnce`; only the
 * dispatch differs by seam — production enqueues (injected here), the unit-spec
 * default resolves inline. So this asserts the production dispatch: a
 * `MovieDetailsComplete` for an unresolved film lands a `ResolveTmdb` task
 * carrying the director hint `directorWalk` needs.
 */
class MovieServiceResolveTaskSpec extends AnyFlatSpec with Matchers {

  private val deadTmdb = new TmdbClient(http = RoutingHttpFetch.dead("unused"), apiKey = None)

  private def serviceEnqueueing(queue: InMemoryTaskQueue, cache: MovieCache): MovieService =
    new MovieService(
      cache, new InProcessEventBus(), deadTmdb,
      enqueueResolveTmdb = Some((title, year, orig, dir) => {
        queue.enqueue(
          TaskType.ResolveTmdb,
          EnrichTaskKeys.resolveTmdbDedup(title, year),
          EnrichTaskKeys.resolveTmdbPayload(title, year, dir, orig))
        ()
      }))

  "onMovieDetailsComplete" should "enqueue a ResolveTmdb task carrying the director hint for an unresolved film" in {
    val queue = new InMemoryTaskQueue()
    val svc   = serviceEnqueueing(queue, new CaffeineMovieCache(new InMemoryMovieRepo()))

    svc.onMovieDetailsComplete(
      MovieDetailsComplete("Interstellar", Some(2014), originalTitle = None, director = Some("Christopher Nolan")))

    val task = queue.claim("w", 1.minute, Instant.now()).getOrElse(fail("no ResolveTmdb task enqueued"))
    task.taskType                          shouldBe TaskType.ResolveTmdb
    task.dedupKey                          shouldBe "resolve-tmdb|Interstellar|2014"
    EnrichTaskKeys.titleOf(task.payload)   shouldBe "Interstellar"
    EnrichTaskKeys.yearOf(task.payload)    shouldBe Some(2014)
    EnrichTaskKeys.directorOf(task.payload) shouldBe Some("Christopher Nolan")
  }

  it should "not enqueue for a film already resolved (no churn, no phantom queue place)" in {
    val queue = new InMemoryTaskQueue()
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.put(cache.keyOf("Resolved Film", Some(2020)), MovieRecord(tmdbId = Some(99)))
    val svc = serviceEnqueueing(queue, cache)

    svc.onMovieDetailsComplete(
      MovieDetailsComplete("Resolved Film", Some(2020), originalTitle = None, director = None))

    queue.monitor().active shouldBe empty
  }
}
