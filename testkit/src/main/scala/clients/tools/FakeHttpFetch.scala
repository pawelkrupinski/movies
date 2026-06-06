package clients.tools

import tools.HttpFetch

import java.net.URI
import java.nio.file.{Files, Paths}
import java.util.concurrent.CompletableFuture

class FakeHttpFetch(fixtureDir: String) extends HttpFetch {
  val fixtureRoot = "test/resources/fixtures/" + fixtureDir

  override def get(url: String): String = new String(readBytes(url, body = None), "UTF-8")

  /** Raw fixture bytes — the on-disk file undecoded. Mirrors
   *  `RealHttpFetch.getBytes` so a charset-quirky client (Kino Charlie, raw
   *  ISO-8859-2) replays through the same decode path it runs in production. */
  override def getBytes(url: String): Array[Byte] = readBytes(url, body = None)

  /** POST is recorded by `RecordingHttpFetch.post` with a body-hash
   *  suffix on the filename — used to distinguish, for example, IMDb's
   *  GraphQL endpoint (`https://caching.graphql.imdb.com/`) where every
   *  call hits the same URL but the request body varies per imdbId.
   *  Mirror the same hash here so the test can replay GraphQL responses. */
  override def post(url: String, body: String, contentType: String): String =
    new String(readBytes(url, body = Some(body)), "UTF-8")

  override def getAsync(url: String): CompletableFuture[String] =
    try CompletableFuture.completedFuture(get(url))
    catch { case e: Exception =>
      val f = new CompletableFuture[String]()
      f.completeExceptionally(e)
      f
    }

  private def readBytes(url: String, body: Option[String]): Array[Byte] = {
    val uri  = new URI(url)
    val path = uri.getPath.stripPrefix("/")
    val base = s"$fixtureRoot/${uri.getHost}/$path"
    // Some fixtures (Rialto) were recorded under URLs with trailing slashes
    // and use the hidden-file-inside-directory convention (`wydarzenie/.NNN`);
    // others (Apollo detail pages) want the slash stripped so the fixture
    // can sit at the parent level (`kino/<slug>.content` alongside other
    // slugs in `kino/`). Try both forms — slashed base first (preserves
    // existing fixtures), then slash-stripped base.
    val trimmedBase = base.stripSuffix("/")
    // Match `RecordingHttpFetch.fileFor`: try the query-fingerprinted
    // form first so search-style endpoints (TMDB `/3/search/movie?query=…`,
    // Filmweb `/api/v1/live/search?query=…`) find the per-query file
    // recorded for THIS specific query. Fall back to the bare path for
    // older fixtures and for path-only URLs (no query, e.g. cinema HTML
    // pages) where one file per path is enough.
    //
    // Two fingerprint shapes coexist in the corpus because of how
    // RecordingHttpFetch handled an empty query historically:
    //   - URLs with `?api_key=…` only → stripped to "" → hashCode 0
    //     → fixture written as `<path>.0` (TMDB `/external_ids`, etc.).
    //   - URLs with no query at all  → suffix omitted entirely → fixture
    //     written as `<path>` (IMDb GraphQL POSTs at the root path).
    // After the v4-bearer TMDB move the URLs now match the second
    // shape, so emit BOTH candidate sets when there's no raw query —
    // one with the `.0` suffix (existing TMDB fixtures) and one bare
    // (existing IMDb / cinema-HTML fixtures).
    val rawQuery     = Option(uri.getRawQuery)
    val querySuffix  = s".${RecordingHttpFetch.stableQueryFingerprint(rawQuery.getOrElse(""))}"
    val bodySuffix   = body.map(b => s".${b.hashCode.toHexString}").getOrElse("")
    val key          = s"$querySuffix$bodySuffix"
    val bareKey      = bodySuffix  // no query → no fingerprint, just body hash (if any)
    val candidates = Seq(
      s"$base$key",
      s"$base$key.html",
      s"$base$key.json",
      s"$base$key.content",
      s"$base$bareKey",
      s"$base$bareKey.html",
      s"$base$bareKey.json",
      s"$base$bareKey.content",
      base,
      s"$base.html",
      s"$base.json",
      s"$base.content",
      s"$trimmedBase$key",
      s"$trimmedBase$key.html",
      s"$trimmedBase$key.json",
      s"$trimmedBase$key.content",
      s"$trimmedBase$bareKey",
      s"$trimmedBase$bareKey.html",
      s"$trimmedBase$bareKey.json",
      s"$trimmedBase$bareKey.content",
      trimmedBase,
      s"$trimmedBase.html",
      s"$trimmedBase.json",
      s"$trimmedBase.content"
    ).distinct
    candidates
      .map(Paths.get(_))
      .find(p => Files.exists(p) && Files.isRegularFile(p))
      .map(p => Files.readAllBytes(p))
      .getOrElse(throw new java.io.FileNotFoundException(
        s"No fixture file for $url — tried:\n  ${candidates.mkString("\n  ")}"
      ))
  }
}
