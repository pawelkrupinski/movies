package controllers

import org.apache.pekko.NotUsed
import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import org.apache.pekko.stream.scaladsl.Source
import play.api.Mode
import play.api.libs.json.Json
import play.api.mvc._
import services.movies.{MovieRepository, StoredMovieRecord}
import services.staging.{StagingRecord, StagingRepository}

import scala.concurrent.ExecutionContext

/**
 * Dev-only Server-Sent Events feed of `movies` change-stream events for the
 * /debug live view. Each connected browser opens its OWN change stream (the
 * page is a low-traffic admin tool); the stream is never opened in prod — the
 * endpoint 404s there like the rest of /debug — so the worker's collection is
 * not watched 24/7 from the web side.
 *
 * On each change the affected row is rendered server-side via the same
 * `_debugRow` / `_stagingRow` partials the page uses, so a live-inserted row is
 * byte-identical to the initial render and no row markup is duplicated in JS. A
 * delete carries only the `_id`, so the page can drop a merged-away row.
 *
 * Two collections are watched: `movies` (the corpus table) and `pending_movies`
 * (the staging table), with `staging-*`-typed frames for the latter so the page
 * routes each to the right table.
 */
class DebugStreamController(
  cc:               ControllerComponents,
  movieRepository:        MovieRepository,
  stagingRepository: StagingRepository,
  environment:      Mode
)(using mat: Materializer) extends AbstractController(cc) {

  def stream: Action[AnyContent] = Action {
    DevMode.gate(environment)(Ok.chunked(eventSource()).as("text/event-stream"))
  }

  /** SSE frame for an upserted row: render `_debugRow` to HTML and ship it with
   *  the row's `_id` so the page can replace-or-insert it. The row's details cell
   *  ships empty (lazily fetched on expand), so no cinema-URL map is needed. */
  private[controllers] def upsertFrame(row: StoredMovieRecord): String = {
    implicit val city: models.City = models.City.all.head
    val html = views.html._debugRow(row).body
    s"data: ${Json.stringify(Json.obj("type" -> "upsert", "id" -> StoredMovieRecord.idOf(row), "html" -> html))}\n\n"
  }

  /** SSE frame for a deleted row: just the `_id`, so the page drops it. */
  private[controllers] def deleteFrame(id: String): String =
    s"data: ${Json.stringify(Json.obj("type" -> "delete", "id" -> id))}\n\n"

  /** SSE frame for an upserted staging row: render `_stagingRow` and ship it with
   *  the row's `pending_movies` `_id` so the page can replace-or-insert it. */
  private[controllers] def stagingUpsertFrame(row: StagingRecord): String = {
    val html = views.html._stagingRow(row).body
    s"data: ${Json.stringify(Json.obj("type" -> "staging-upsert", "id" -> StagingRecord.idFor(row.cinema, row.title, row.year), "html" -> html))}\n\n"
  }

  /** SSE frame for a removed staging row (the film graduated): just the `_id`. */
  private[controllers] def stagingDeleteFrame(id: String): String =
    s"data: ${Json.stringify(Json.obj("type" -> "staging-delete", "id" -> id))}\n\n"

  /** One change-stream subscription per connection per watched collection, all
   *  closed when the browser disconnects (watchTermination). A Mongo without a
   *  replica set just errors the streams — the page keeps its static tables. */
  private[controllers] def eventSource(): Source[String, NotUsed] = {
    val (queue, source) =
      Source.queue[String](DebugStreamController.BufferSize, OverflowStrategy.dropHead).preMaterialize()
    val watches: Seq[AutoCloseable] = Seq(
      movieRepository.watchChanges(
        onUpsert = row => { queue.offer(upsertFrame(row)); () },
        onDelete = id  => { queue.offer(deleteFrame(id)); () }
      ),
      stagingRepository.watchChanges(
        onUpsert = row => { queue.offer(stagingUpsertFrame(row)); () },
        onDelete = id  => { queue.offer(stagingDeleteFrame(id)); () }
      )
    ).flatten
    source.watchTermination() { (_, done) =>
      done.onComplete(_ => watches.foreach(_.close()))(using ExecutionContext.global)
      NotUsed
    }
  }
}

object DebugStreamController {
  private val BufferSize = 256
}
