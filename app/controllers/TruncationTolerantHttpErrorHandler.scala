package controllers

import org.apache.pekko.http.scaladsl.model.EntityStreamException
import play.api.http.{DefaultHttpErrorHandler, HttpErrorConfig}
import play.api.mvc.RequestHeader
import play.api.routing.Router
import play.api.{Environment, Logger, UsefulException}
import play.core.SourceMapper

/**
 * Wraps Play's `DefaultHttpErrorHandler` so client-side body truncation
 * doesn't surface as ERROR (and so doesn't reach Sentry).
 *
 * Pekko throws `EntityStreamException` when the underlying connection
 * closes before the HTTP body finishes streaming in. The browser does
 * exactly that on page unload when `navigator.sendBeacon()` posts a
 * payload that the OS hasn't fully flushed yet — `/uptime/img-event`
 * gets the truncated body, parsing fails, Play wraps the cause in an
 * `UnexpectedException` and the default handler logs ERROR. The user
 * already navigated away; the request can't be salvaged; there's
 * nothing for us to fix on the server side.
 *
 * Log it at WARN with enough detail to spot if it shifts from "rare
 * sendBeacon timing" to "all uploads breaking", and let the default
 * 500 response stand — the client is gone, the body wouldn't be read.
 */
class TruncationTolerantHttpErrorHandler(
  environment:   Environment,
  config:        HttpErrorConfig,
  sourceMapper:  Option[SourceMapper],
  router:        => Option[Router]
) extends DefaultHttpErrorHandler(config, sourceMapper, router) {

  private val logger = Logger(classOf[TruncationTolerantHttpErrorHandler])

  override protected def logServerError(request: RequestHeader, usefulException: UsefulException): Unit = {
    if (isClientTruncation(usefulException.cause)) {
      logger.warn(
        s"Client truncated request body for ${request.method} ${request.uri}: " +
          s"${usefulException.cause.getClass.getSimpleName}: ${Option(usefulException.cause.getMessage).getOrElse("")}"
      )
    } else super.logServerError(request, usefulException)
  }

  // EntityStreamException is the marker Pekko's HTTP parser raises for
  // an entity that ended before its declared length. We also peek a few
  // levels down — Play wraps the parser exception inside an
  // `UnexpectedException`, so the immediate cause is the wrapper and
  // the *real* cause is its own `getCause`.
  private[controllers] def isClientTruncation(cause: Throwable): Boolean = {
    @scala.annotation.tailrec
    def walk(t: Throwable, depth: Int): Boolean =
      if (t == null || depth > 4) false
      else if (t.isInstanceOf[EntityStreamException]) true
      else walk(t.getCause, depth + 1)
    walk(cause, 0)
  }
}
