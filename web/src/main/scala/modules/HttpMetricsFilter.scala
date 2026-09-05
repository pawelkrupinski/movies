package modules

import play.api.mvc.{EssentialAction, EssentialFilter, Result}
import services.metrics.WebHttpMetrics

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/**
 * Counts and times every inbound request into [[WebHttpMetrics]].
 *
 * An `EssentialFilter` rather than per-controller instrumentation because the
 * point is that NOTHING can be added to the app and quietly go unmeasured — a
 * new controller, a redirect, an asset, a 404 from a route that doesn't exist.
 * Instrumenting controllers would have covered the handlers we remembered to
 * touch, which is exactly the set that isn't the problem.
 *
 * Wired OUTERMOST in `AppComponents.httpFilters` (ahead of Play's own
 * CSRF/allowed-hosts/security-headers filters and ahead of gzip). That position
 * is deliberate: the number this publishes is what the user waited for,
 * including gzip compression of a 4 MB listing body, and a request rejected by
 * `AllowedHostsFilter` still counts as a 400 the tier served. An inner position
 * would silently drop those rejections from the error rate.
 *
 * The clock stops when the RESPONSE HEADER is produced, not when the body has
 * finished streaming. Play hands back the result before the body flows, and the
 * app serves open-ended SSE streams (`/debug/stream`, `/uptime/stream`) whose
 * bodies live for minutes — timing those to completion would put a permanent
 * multi-minute tail in the histogram and make the p99 meaningless.
 *
 * @param nanoTime injected so the spec can drive a deterministic duration; the
 *                 default is the monotonic clock, which is the right one here —
 *                 wall-clock would let an NTP step produce a negative latency.
 */
class HttpMetricsFilter(
  metrics:  WebHttpMetrics,
  nanoTime: () => Long = () => System.nanoTime()
)(using executionContext: ExecutionContext) extends EssentialFilter {

  override def apply(next: EssentialAction): EssentialAction = EssentialAction { request =>
    if (!WebHttpMetrics.isMeasured(request)) next(request)
    else {
      val startedNanos = nanoTime()
      def elapsedSeconds: Double = (nanoTime() - startedNanos).toDouble / 1e9

      next(request)
        .map { result =>
          metrics.record(request, result.header.status, elapsedSeconds,
                         HttpMetricsFilter.wireLength(result))
          result
        }(using executionContext)
        // A handler whose future FAILS never reaches the branch above: Play's
        // error handler turns it into a 500 out in the server layer, which is
        // outside the filter chain. Without this branch the app's own 500s —
        // the single most important thing on the error-rate panel — would be
        // the only responses that never got counted. The exception is rethrown
        // untouched so the real error handler still renders the page.
        .recoverWith { case NonFatal(e) =>
          metrics.record(request, HttpMetricsFilter.UnhandledErrorStatus, elapsedSeconds)
          Future.failed(e)
        }(using executionContext)
    }
  }
}

object HttpMetricsFilter {
  /** What an escaped exception is recorded as — matching the 500 Play's own
   *  error handler will end up returning for it. */
  val UnhandledErrorStatus: Int = 500

  /** The bytes this response puts on the wire, when it says.
   *
   *  READ HERE AND NOWHERE ELSE, because of where this filter sits. It is
   *  wired OUTERMOST, so by the time the result comes back up the chain gzip
   *  has already run — this Content-Length is the COMPRESSED size, which is
   *  what the visitor actually downloads. Measured anywhere inside the gzip
   *  filter it would report the uncompressed body and overstate every page by
   *  roughly an order of magnitude.
   *
   *  `None` for a body with no declared length — chunked responses and the SSE
   *  streams. Recording a zero for those would drag every percentile down and
   *  make the metric say the opposite of the truth. */
  def wireLength(result: Result): Option[Long] =
    result.header.headers
      .get(play.api.http.HeaderNames.CONTENT_LENGTH)
      .flatMap(v => scala.util.Try(v.toLong).toOption)
      .orElse(result.body.contentLength)
      .filter(_ >= 0)
}
