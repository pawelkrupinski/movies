package tools

import scala.collection.mutable

/**
 * Shared stub `HttpFetch` for unit-test specs that need a controlled
 * external surface without standing up the full fixture replay machinery
 * in `clients.tools.FakeHttpFetch`.
 *
 * Routes each call by URL substring against `routes`, returning the
 * matching body or throwing on no match. Both `get` and `post` consult
 * the same routes by default — convenient for flows like Filmweb whose
 * search and rating endpoints mix the two. Pass `getOnly = true` to make
 * any POST throw instead (the Facebook OAuth flow is GET-only and should
 * fail loudly if a caller ever issues one).
 *
 * Every call is recorded into `calls` as a `(method, url)` tuple so
 * tests can assert request order or count. POST `body` + `contentType`
 * additionally land in `postBodies` for specs that need to inspect them
 * (e.g. the Google OAuth token exchange).
 *
 * For dependencies the test should never reach, use
 * `RoutingHttpFetch.dead(label)` — it throws on the first call with the
 * given label, useful for wiring `deadFilmweb`-style "you shouldn't
 * touch me" clients into a test under composition.
 *
 * `unroutedIsNotFound` picks what an unrouted URL MEANS, a distinction that
 * only started to matter once the enrichment clients stopped swallowing every
 * failure into `None` (see [[EnrichmentRead]]):
 *
 *   - `false` (default) — a loud non-HTTP error: "this test wired something
 *     wrong / issued a call it shouldn't have". Several specs lean on that as a
 *     tripwire, so it stays the default.
 *   - `true` — the 404 a real site returns for a page it doesn't have. Right for
 *     specs modelling an upstream that simply has no entry for this film, where
 *     a generic error would instead claim the site is broken and abort a probe
 *     ladder that was supposed to move on to its next candidate.
 */
class RoutingHttpFetch(
  routes: Map[String, String],
  getOnly: Boolean = false,
  unroutedIsNotFound: Boolean = false
) extends HttpFetch {
  val calls:      mutable.ListBuffer[(String, String)]         = mutable.ListBuffer.empty
  val postBodies: mutable.ListBuffer[(String, String, String)] = mutable.ListBuffer.empty

  private def lookup(url: String): String =
    routes.collectFirst { case (frag, body) if url.contains(frag) => body }
      .getOrElse {
        if (unroutedIsNotFound) UpstreamNotFound(url)
        else throw new RuntimeException(s"unstubbed URL: $url")
      }

  override def get(url: String): String = {
    calls += (("GET", url))
    lookup(url)
  }

  override def post(url: String, body: String, contentType: String): String = {
    calls      += (("POST", url))
    postBodies += ((url, body, contentType))
    if (getOnly) throw new RuntimeException(s"unexpected POST on getOnly fetch: $url")
    lookup(url)
  }
}

object RoutingHttpFetch {
  /** Fetch that throws on every call. For dependencies a test wires for
   *  composition but should never reach (e.g. the IMDb client a
   *  TMDB-only test doesn't exercise). */
  def dead(label: String = "unused"): HttpFetch = new HttpFetch {
    override def get(url: String): String =
      throw new RuntimeException(s"$label: $url")
    override def post(url: String, body: String, contentType: String): String =
      throw new RuntimeException(s"$label: $url")
  }
}
