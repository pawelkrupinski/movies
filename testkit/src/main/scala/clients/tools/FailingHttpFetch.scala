package clients.tools

import tools.{HttpFetch, HttpStatusException}

/** An [[tools.HttpFetch]] whose every request fails with an [[HttpStatusException]] —
 *  the same typed exception `RealHttpFetch` throws on a non-2xx response. Used to
 *  assert that a scraper PROPAGATES a fetch failure (so it surfaces red on the
 *  uptime page) instead of swallowing it into an empty list (which reads as a
 *  successful "0 showtimes" scrape — white, indistinguishable from a genuinely
 *  film-dormant venue). Defaults to 503, the shared-hosting overload code that
 *  motivated the guard.
 *
 *  POST fails the same way GET does: a client whose listing call is a POST (AMC's
 *  GraphQL day query) needs the identical guard, and a fake that answered POST
 *  with a DIFFERENT exception type would let such a client's failure path pass
 *  its test while behaving differently in production. */
class FailingHttpFetch(status: Int = 503) extends HttpFetch {
  override def get(url: String): String =
    throw new HttpStatusException(status, "GET", url, retryAfter = None)

  override def post(url: String, body: String, contentType: String): String =
    throw new HttpStatusException(status, "POST", url, retryAfter = None)
}
