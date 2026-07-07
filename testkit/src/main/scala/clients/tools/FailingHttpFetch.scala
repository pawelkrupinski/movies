package clients.tools

import tools.{GetOnlyHttpFetch, HttpStatusException}

/** An [[tools.HttpFetch]] whose every GET fails with an [[HttpStatusException]] —
 *  the same typed exception `RealHttpFetch` throws on a non-2xx response. Used to
 *  assert that a scraper PROPAGATES a fetch failure (so it surfaces red on the
 *  uptime page) instead of swallowing it into an empty list (which reads as a
 *  successful "0 showtimes" scrape — white, indistinguishable from a genuinely
 *  film-dormant venue). Defaults to 503, the shared-hosting overload code that
 *  motivated the guard. */
class FailingHttpFetch(status: Int = 503) extends GetOnlyHttpFetch {
  override def get(url: String): String =
    throw new HttpStatusException(status, "GET", url, retryAfter = None)
}
