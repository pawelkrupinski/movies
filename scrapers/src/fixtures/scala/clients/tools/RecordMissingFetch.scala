package clients.tools

import tools.{HttpFetch, RealHttpFetch}

import java.net.URI

/** Replay-first HTTP that fills only the GAPS — and only for hosts you name.
 *
 *  Every request is served from the on-disk fixtures (`FakeHttpFetch`). When a
 *  fixture is MISSING:
 *    - if the URL's host is in `recordHosts`, fetch it LIVE and WRITE the fixture
 *      (`RecordingHttpFetch`), filling the gap with real data;
 *    - otherwise, the fixture-miss exception propagates unchanged — exactly as a
 *      pure replay would fail.
 *
 *  This is what makes it safe to refresh a *pinned, historical* corpus: existing
 *  fixtures are served untouched (no re-write → no drift to today's data), the
 *  cinema-scrape corpus can never grow because a missing cinema fixture still
 *  throws (replay-only), and only genuinely-new requests — e.g. the changed
 *  search-title queries this title-pipeline change produced — hit the network
 *  and land on disk. `recordHosts` matches a host exactly or as a domain suffix
 *  (`"imdb.com"` covers `caching.graphql.imdb.com`).
 *
 *  Used by [[RefreshExternalFixtures]] with the external-metadata hosts only
 *  (themoviedb / filmweb / rottentomatoes / imdb / metacritic). */
class RecordMissingFetch(
  fixtureDir:  String,
  recordHosts: Set[String],
  live:        HttpFetch = new RealHttpFetch()
) extends HttpFetch {
  private val fake     = new FakeHttpFetch(fixtureDir)
  private val recorder = new RecordingHttpFetch(fixtureDir, live)

  private def recordable(url: String): Boolean = {
    val host = Option(new URI(url).getHost).getOrElse("")
    recordHosts.exists(h => host == h || host.endsWith("." + h))
  }

  override def get(url: String): String =
    try fake.get(url) catch { case _: Throwable if recordable(url) => recorder.get(url) }

  override def post(url: String, body: String, contentType: String): String =
    try fake.post(url, body, contentType) catch { case _: Throwable if recordable(url) => recorder.post(url, body, contentType) }

  // Binary fetches (posters) are never an external-metadata gap — serve from
  // fixtures only, so a stray miss surfaces instead of silently going live.
  override def getBytes(url: String): Array[Byte] = fake.getBytes(url)
}
