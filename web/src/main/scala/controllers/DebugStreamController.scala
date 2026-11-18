package controllers

import org.apache.pekko.NotUsed
import org.apache.pekko.stream.{Materializer, OverflowStrategy}
import org.apache.pekko.stream.scaladsl.Source
import play.api.Mode
import play.api.mvc._

import scala.concurrent.ExecutionContext

/**
 * Dev-only Server-Sent Events feed of `movies` change-stream events for the
 * /debug live view. Each connected browser opens its OWN change stream (the
 * page is a low-traffic admin tool); the stream is never opened in prod — the
 * endpoint 404s there like the rest of /debug — so the worker's collection is
 * not watched 24/7 from the web side.
 *
 * What each change says is [[DebugRowFeed]]'s: the affected row rendered
 * server-side, or a delete's `_id`.
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

  private[controllers] def eventSource(request: RequestHeader): Source[String, NotUsed] = {
    val country = debugCountries.resolve(request)
    val feed    = new DebugRowFeed(debugCountries.stackFor(country), country)
    val (queue, source) =
      Source.queue[String](DebugStreamController.BufferSize, OverflowStrategy.dropHead).preMaterialize()
    val watches = feed.open(frame => { queue.offer(frame); () }).toSeq
    source.watchTermination() { (_, done) =>
      done.onComplete(_ => watches.foreach(_.close()))(using ExecutionContext.global)
      NotUsed
    }
  }
}

object DebugStreamController {
  private val BufferSize = 256
}
