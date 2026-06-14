package clients.tools

import tools.HttpFetch

import java.io.File
import java.net.URI
import java.nio.file.Files

/** Pass-through `HttpFetch` that also writes every response body to a
 *  fixture file under `test/resources/fixtures/$fixtureDirectory/`. Used by
 *  `RecordAllDataToFixture` to capture every byte every client would
 *  fetch in normal operation, on disk and replayable via
 *  `FakeHttpFetch`.
 *
 *  POST is supported (not just GET) so the IMDb GraphQL endpoint —
 *  `ImdbClient.lookup` POSTs to caching.graphql.imdb.com — gets
 *  recorded too. POST bodies don't fit naturally into the URL-keyed
 *  fixture tree, so we key the POST file by the URL plus a body hash;
 *  in practice the GraphQL endpoint's POST body uniquely identifies
 *  the imdbId, so one file per id is what we want.
 *
 *  The delegate is any `HttpFetch`, not just `RealHttpFetch`: a
 *  Zyte-routed chain (`MultikinoClient.fetchFor` / `ZyteFallback.fetchFor`)
 *  is an `HttpFetch` whose Zyte leg fetches through its own client, bypassing
 *  the recorder when recording sits *inside* the chain as the direct fallback.
 *  Wrapping the whole chain instead records the response keyed by the request
 *  (target) URL regardless of which leg served it. See `RecorderZyteCaptureSpec`.
 */
class RecordingHttpFetch(fixtureDirectory: String, delegate: HttpFetch) extends HttpFetch {
  val fixtureRoot = "test/resources/fixtures/" + fixtureDirectory

  override def get(url: String): String = {
    val content = delegate.get(url)
    write(fileFor(url), content)
    content
  }

  override def post(url: String, body: String, contentType: String): String = {
    val content = delegate.post(url, body, contentType)
    write(fileFor(url, Some(body)), content)
    content
  }

  private def fileFor(url: String, body: Option[String] = None): File = {
    val uri = new URI(url)
    val path = uri.getPath.stripPrefix("/")
    // Suffix with a fingerprint of the query string, so distinct queries
    // to the same path (`/3/search/movie?query=Prada` vs `…?query=Belle`,
    // `/api/v1/live/search?query=Foo` vs `…?query=Bar`) get distinct
    // fixture files instead of overwriting each other. `RecordingHttpFetch`
    // used to key by path alone — Prada's search response was therefore
    // lost when the next query overwrote `/3/search/movie`.
    val querySuffix = Option(uri.getRawQuery).map(q => s".${stableQueryFingerprint(q)}").getOrElse("")
    val bodySuffix  = body.map(b => s".${b.hashCode.toHexString}").getOrElse("")
    new File(s"$fixtureRoot/${uri.getHost}/$path$querySuffix$bodySuffix")
  }

  /** Hash the query string with rotating auth parameters (`api_key`,
   *  `access_token`, …) stripped, so the same query produces the same
   *  fingerprint regardless of who recorded it. Without this, every
   *  developer's TMDB key would produce a different hash and fixtures
   *  recorded with one key wouldn't be findable by another. */
  private def stableQueryFingerprint(rawQuery: String): String =
    RecordingHttpFetch.stableQueryFingerprint(rawQuery)

  /** Write the recorded body to disk, resolving filesystem-path conflicts
   *  between URLs where one is a prefix of the other. Two TMDB endpoints
   *  illustrate the case:
   *
   *    GET /3/movie/931285             →  /3/movie/931285        (leaf file)
   *    GET /3/movie/931285/external_ids →  /3/movie/931285/external_ids
   *
   *  The second wants `/3/movie/931285` to be a *directory*; the first
   *  wrote it as a *file*. Whichever runs second hits `IOException: Not a
   *  directory` / `Is a directory`, the response body never lands on
   *  disk, `MovieService.runTmdbStage` catches the IOException and
   *  schedules a 30-s retry — which then races against the worker's
   *  shutdown and either re-fails or gets rejected silently. Recording
   *  loses every fixture for the affected row.
   *
   *  Resolution: when a write needs `<x>` to be a directory but `<x>` is
   *  a regular file, rename the file to `<x>.content` (so it lives as a
   *  sibling). When a write needs `<x>` as a file but `<x>` is already a
   *  directory, write to `<x>.content` instead. `FakeHttpFetch.get` already
   *  tries `base`, `base.html`, `base.content` in order, so the read path
   *  finds the renamed file without any further change. */
  private def write(file: File, content: String): Unit = {
    renameAncestorIfFile(file)
    file.getParentFile.mkdirs()
    val target = if (file.exists() && file.isDirectory) contentSibling(file) else file
    target.createNewFile()
    Files.write(target.toPath, content.getBytes("UTF-8"))
  }

  /** Walk up from `file` to find the deepest existing ancestor. If it's
   *  a regular file (would block `mkdirs` of `file.getParentFile`),
   *  rename it to `<name>.content` so the path can become a directory. */
  private def renameAncestorIfFile(file: File): Unit = {
    var ancestor: File = file.getParentFile
    while (ancestor != null && !ancestor.exists()) ancestor = ancestor.getParentFile
    if (ancestor != null && ancestor.isFile) {
      val renamed = contentSibling(ancestor)
      if (renamed.exists()) ancestor.delete()
      else ancestor.renameTo(renamed)
    }
  }

  private def contentSibling(f: File): File =
    new File(f.getParentFile, s"${f.getName}.content")
}

object RecordingHttpFetch {
  /** Query parameters that aren't semantically distinguishing for fixture
   *  replay — stripped before fingerprinting so the fixture filename
   *  stays stable across users, scrape orders, and other rotating
   *  state. Two flavours:
   *
   *  Auth: `api_key`, `apikey`, `access_token`, `token` — rotate per
   *  developer / token refresh.
   *
   *  TMDB year filters: `year`, `primary_release_year` — server-side
   *  filters on `/3/search/movie`. The recording runs scrapers in
   *  parallel on the fetch executor, so whichever cinema completes
   *  `recordCinemaScrape` first determines the canonical CacheKey's
   *  year (Multikino's `year=None` wins if it finishes ahead of
   *  CinemaCity's `year=Some(2026)`, and vice versa). A sequential
   *  test run gets a different winner, so the URL `TmdbClient.search`
   *  emits has different year parameters at recording vs test. The
   *  *response* doesn't materially differ for our parser — pickBest
   *  filters the results anyway — so we fold both URL flavours onto a
   *  single fixture file and avoid the order-of-scrape coupling. */
  private val IgnoredParameters = Set(
    "api_key", "apikey", "access_token", "token",
    "year", "primary_release_year"
  )

  /** Hash the meaningful (non-ignored) parts of a raw query string.
   *  Shared between `RecordingHttpFetch` (write side) and
   *  `FakeHttpFetch` (read side) so writer and reader agree on which
   *  file to look up. */
  def stableQueryFingerprint(rawQuery: String): String = {
    val meaningful = rawQuery
      .split("&")
      .filterNot(p => IgnoredParameters.exists(name => p.startsWith(s"$name=")))
      .sorted  // canonicalise parameter order so URL-builder differences don't matter
    meaningful.mkString("&").hashCode.toHexString
  }
}
