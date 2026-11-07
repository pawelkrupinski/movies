package clients.tools

import tools.HttpFetch

import java.net.URI

/**
 * An `HttpFetch` that answers by matching a FRAGMENT of the requested URL — the shape
 * every hand-stubbed API spec had reinvented ("`haswbstatement` → this body,
 * `wbgetentities` → that one"). Routes are tried in order, so a more specific fragment
 * can precede a general one.
 *
 * It PARSES the URL first, through the same `URI.create` that `RealHttpFetch` calls when
 * it builds a request. That is the whole reason this lives here rather than in each spec:
 * a stub that matches on `String.contains` accepts URLs no real fetch could ever send, so
 * a client that builds an unsendable one passes its tests and fails only in production.
 *
 * `WikidataClient` did exactly that. Its `wbgetentities` calls joined Q-IDs — and named
 * props — with Wikidata's literal `|` separator, which `URI.create` rejects outright
 * (`IllegalArgumentException: Illegal character in query`). Every call threw before a
 * byte left the process, so a whole rung of the imdbId ladder was dead in production
 * while four specs stubbed around it and stayed green.
 */
class UrlFragmentHttpFetch(routes: Seq[(String, String)]) extends HttpFetch {

  override def get(url: String): String = {
    // Not a formality: this is the call that rejects a URL production could never send.
    URI.create(url)
    routes.collectFirst { case (fragment, body) if url.contains(fragment) => body }
      .getOrElse(throw new java.io.FileNotFoundException(
        s"no stubbed response for $url — routes: ${routes.map(_._1).mkString(", ")}"))
  }

  /** Fakes route by URL alone; the headers a client adds (User-Agent, auth) are its own
   *  business and no route has ever needed to see them. */
  override def get(url: String, headers: Map[String, String]): String = get(url)

  override def post(url: String, body: String, contentType: String): String =
    throw new UnsupportedOperationException(s"UrlFragmentHttpFetch does not stub POST (url=$url)")
}

object UrlFragmentHttpFetch {
  def apply(routes: (String, String)*): UrlFragmentHttpFetch = new UrlFragmentHttpFetch(routes.toSeq)
}
