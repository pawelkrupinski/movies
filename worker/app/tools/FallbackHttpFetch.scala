package tools

import play.api.Logging

import scala.collection.mutable

/**
 * Tries each backend in order, returning the first successful body.
 * Logs each failure as a warning and falls through to the next one;
 * if every backend fails, throws a single composite exception that
 * names each failure.
 *
 * Used by Multikino's composition: Zyte primary → direct fetch as
 * last resort. None of the backends know about each other — each is
 * just an `HttpFetch`.
 *
 * Single-backend lists are a degenerate case: prefer just using that
 * backend directly. Empty lists are a wiring bug — throws at
 * construction so misconfigured chains don't fail silently in
 * production on the first request.
 */
class FallbackHttpFetch(backends: Seq[(String, HttpFetch)]) extends HttpFetch with Logging {
  require(backends.nonEmpty, "FallbackHttpFetch needs at least one backend")

  override def get(url: String): String = tryEach("get", url, _.get(url))

  override def post(url: String, body: String, contentType: String): String =
    tryEach("post", url, _.post(url, body, contentType))

  private def tryEach(verb: String, url: String, call: HttpFetch => String): String = {
    val failures        = mutable.ListBuffer.empty[String]
    var result: Option[String] = None
    val it              = backends.iterator
    while (result.isEmpty && it.hasNext) {
      val (name, backend) = it.next()
      try result = Some(call(backend))
      catch {
        case t: Throwable =>
          val msg = s"$name: ${t.getClass.getSimpleName}: ${t.getMessage}"
          logger.warn(s"FallbackHttpFetch $verb $url — $msg; trying next backend")
          failures += msg
      }
    }
    result.getOrElse {
      throw new RuntimeException(
        s"All ${backends.size} backends failed for $verb $url:\n  " +
        failures.mkString("\n  ")
      )
    }
  }
}
