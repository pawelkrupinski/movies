package controllers

import org.apache.pekko.NotUsed
import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import org.apache.pekko.stream.scaladsl.Source
import play.api.Mode
import play.api.libs.json.Json
import play.api.mvc._
import services.movies.StoredMovieRecord

import scala.concurrent.ExecutionContext

/**
 * Dev-only Server-Sent Events feed of `movies` change-stream events for the
 * /debug live view. Each connected browser opens its OWN change stream (the
 * page is a low-traffic admin tool); the stream is never opened in prod — the
 * endpoint 404s there like the rest of /debug — so the worker's collection is
 * not watched 24/7 from the web side.
 *
 * On each change the affected row is rendered server-side via the same
 * `_debugRow` partial the page uses, so a live-inserted row is
 * byte-identical to the initial render and no row markup is duplicated in JS. A
 * delete carries only the `_id`, so the page can drop a merged-away row.
 */
class DebugStreamController(
  cc:               ControllerComponents,
  // The per-country debug stacks; the stream watches the SELECTED country's
  // `movies` (the sticky `debugCountry` cookie the /debug page
  // set carries the selection here, since an EventSource sends no query string).
  debugCountries:   DebugCountries,
  environment:      Mode
)(using mat: Materializer, latestYear: services.movies.LatestTitleYear) extends AbstractController(cc) {

  def stream: Action[AnyContent] = Action { request =>
    DevMode.gate(environment)(Ok.chunked(eventSource(request)).as("text/event-stream"))
  }

  // The frames render under the country the connection WATCHES — its stack's own title rules
  // and one of its own cities. They used to render under `City.all.head` (Poznań) and Poland's
  // rules in every deployment, so a UK /debug stream keyed its rows with Polish sanitizing.
  private def frameCity(country: models.Country): models.City = country.cities.head

  /** SSE frame for an upserted row: render `_debugRow` to HTML and ship it with
   *  the row's `_id` so the page can replace-or-insert it. The row's details cell
   *  ships empty (lazily fetched on expand), so no cinema-URL map is needed. */
  private[controllers] def upsertFrame(row: StoredMovieRecord, country: models.Country,
                                       normalizer: services.movies.TitleNormalizer): String = {
    implicit val city: models.City = frameCity(country)
    val html = views.html._debugRow(row, normalizer, latestYear).body
    s"data: ${Json.stringify(Json.obj("type" -> "upsert", "id" -> row.id.value, "html" -> html))}\n\n"
  }

  /** SSE frame for a deleted row: just the `_id`, so the page drops it. */
  private[controllers] def deleteFrame(id: String): String =
    s"data: ${Json.stringify(Json.obj("type" -> "delete", "id" -> id))}\n\n"

  /** One change-stream subscription per connection per watched collection, all
   *  closed when the browser disconnects (watchTermination). A Mongo without a
   *  replica set just errors the streams — the page keeps its static tables. */
  private[controllers] def eventSource(request: RequestHeader): Source[String, NotUsed] = {
    val country    = debugCountries.resolve(request)
    val stack      = debugCountries.stackFor(country)
    val normalizer = stack.movieRepository.normalizer
    val (queue, source) =
      Source.queue[String](DebugStreamController.BufferSize, OverflowStrategy.dropHead).preMaterialize()
    val watches: Seq[AutoCloseable] = Seq(
      stack.movieRepository.watchChanges(
        onUpsert = row => { queue.offer(upsertFrame(row, country, normalizer)); () },
        onDelete = id  => { queue.offer(deleteFrame(id.value)); () }
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
