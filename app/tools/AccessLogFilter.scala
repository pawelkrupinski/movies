package tools

import org.apache.pekko.stream.Materializer
import play.api.Logging
import play.api.mvc.{Filter, RequestHeader, Result}

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

/**
 * Per-request server-side timing log. One line per page hit, written at
 * INFO under the `tools.AccessLogFilter` logger so it's easy to grep:
 *
 *   `ACCESS GET /kina 200 142ms`
 *
 * Requests with the `/assets/` path prefix are skipped — Bootstrap CSS,
 * posters, every other subresource the browser fans out per page would
 * otherwise drown the meaningful navigation lines.
 *
 * The duration here is server-side only (from when the controller starts
 * to when Play has the `Result` ready to ship). The client's
 * "experienceable" time on top of this includes TLS handshake + RTT +
 * body transfer — the values we wanted to compare desktop vs mobile.
 */
class AccessLogFilter @Inject() (implicit val mat: Materializer, ec: ExecutionContext)
    extends Filter with Logging {
  override def apply(next: RequestHeader => Future[Result])(rh: RequestHeader): Future[Result] = {
    if (rh.path.startsWith("/assets/")) return next(rh)
    val t0 = System.nanoTime()
    next(rh).map { result =>
      val ms = (System.nanoTime() - t0) / 1000000
      logger.info(s"ACCESS ${rh.method} ${rh.uri} ${result.header.status} ${ms}ms")
      result
    }
  }
}
