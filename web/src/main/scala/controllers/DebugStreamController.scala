package controllers

import org.apache.pekko.NotUsed
import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import org.apache.pekko.stream.scaladsl.Source
import play.api.Mode
import play.api.libs.json.Json
import play.api.mvc._
import services.movies.{MovieRepository, StoredMovieRecord}

import scala.concurrent.ExecutionContext

/**
 * Dev-only Server-Sent Events feed of `movies` change-stream events for the
 * /debug live view. Each connected browser opens its OWN change stream (the
 * page is a low-traffic admin tool); the stream is never opened in prod — the
 * endpoint 404s there like the rest of /debug — so the worker's collection is
 * not watched 24/7 from the web side.
 *
 * On each change the affected row is rendered server-side via the same
 * `_debugRow` partial the page uses, so a live-inserted row is byte-identical
 * to the initial render and no row markup is duplicated in JS. A delete carries
 * only the `_id`, so the page can drop a merged-away row.
 */
class DebugStreamController(
  cc:               ControllerComponents,
  movieRepository:        MovieRepository,
  environment:      Mode,
  cinemaSourceUrls: () => Map[String, String]
)(using mat: Materializer) extends AbstractController(cc) {

  def stream: Action[AnyContent] = Action {
    if (environment == Mode.Prod) NotFound("dev-only endpoint")
    else Ok.chunked(eventSource()).as("text/event-stream")
  }

  /** SSE frame for an upserted row: render `_debugRow` to HTML and ship it with
   *  the row's `_id` so the page can replace-or-insert it. */
  private[controllers] def upsertFrame(row: StoredMovieRecord, urls: Map[String, String]): String = {
    implicit val city: models.City = models.City.all.head
    val html = views.html._debugRow(row, urls).body
    s"data: ${Json.stringify(Json.obj("type" -> "upsert", "id" -> StoredMovieRecord.idOf(row), "html" -> html))}\n\n"
  }

  /** SSE frame for a deleted row: just the `_id`, so the page drops it. */
  private[controllers] def deleteFrame(id: String): String =
    s"data: ${Json.stringify(Json.obj("type" -> "delete", "id" -> id))}\n\n"

  /** One change-stream subscription per connection; closed when the browser
   *  disconnects (watchTermination). A Mongo without a replica set just errors
   *  the stream — the page keeps its static table. */
  private[controllers] def eventSource(): Source[String, NotUsed] = {
    val (queue, source) =
      Source.queue[String](DebugStreamController.BufferSize, OverflowStrategy.dropHead).preMaterialize()
    val urls = cinemaSourceUrls()
    val watch: Option[AutoCloseable] = movieRepository.watchChanges(
      onUpsert = row => { queue.offer(upsertFrame(row, urls)); () },
      onDelete = id  => { queue.offer(deleteFrame(id)); () }
    )
    source.watchTermination() { (_, done) =>
      done.onComplete(_ => watch.foreach(_.close()))(using ExecutionContext.global)
      NotUsed
    }
  }
}

object DebugStreamController {
  private val BufferSize = 256
}
