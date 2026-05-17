package clients.tools

import tools.HttpFetch

import java.net.URI
import java.nio.file.{Files, Paths}
import java.util.concurrent.CompletableFuture

class FakeHttpFetch(fixtureDir: String) extends HttpFetch {
  val fixtureRoot = "test/resources/fixtures/" + fixtureDir

  override def get(url: String): String = read(url, body = None)

  /** POST is recorded by `RecordingHttpFetch.post` with a body-hash
   *  suffix on the filename — used to distinguish, for example, IMDb's
   *  GraphQL endpoint (`https://caching.graphql.imdb.com/`) where every
   *  call hits the same URL but the request body varies per imdbId.
   *  Mirror the same hash here so the test can replay GraphQL responses. */
  override def post(url: String, body: String, contentType: String): String =
    read(url, body = Some(body))

  override def getAsync(url: String): CompletableFuture[String] =
    try CompletableFuture.completedFuture(get(url))
    catch { case e: Exception =>
      val f = new CompletableFuture[String]()
      f.completeExceptionally(e)
      f
    }

  private def read(url: String, body: Option[String]): String = {
    val uri  = new URI(url)
    val path = uri.getPath.stripPrefix("/")
    val base = s"$fixtureRoot/${uri.getHost}/$path"
    // Match `RecordingHttpFetch.fileFor`: try the query-fingerprinted
    // form first so search-style endpoints (TMDB `/3/search/movie?query=…`,
    // Filmweb `/api/v1/live/search?query=…`) find the per-query file
    // recorded for THIS specific query. Fall back to the bare path for
    // older fixtures and for path-only URLs (no query, e.g. cinema HTML
    // pages) where one file per path is enough.
    val querySuffix = Option(uri.getRawQuery)
      .map(q => s".${RecordingHttpFetch.stableQueryFingerprint(q)}")
      .getOrElse("")
    val bodySuffix  = body.map(b => s".${b.hashCode.toHexString}").getOrElse("")
    val key = s"$querySuffix$bodySuffix"
    val candidates = Seq(
      s"$base$key",
      s"$base$key.html",
      s"$base$key.json",
      s"$base$key.content",
      base,
      s"$base.html",
      s"$base.json",
      s"$base.content"
    )
    candidates
      .map(Paths.get(_))
      .find(p => Files.exists(p) && Files.isRegularFile(p))
      .map(p => new String(Files.readAllBytes(p), "UTF-8"))
      .getOrElse(throw new java.io.FileNotFoundException(
        s"No fixture file for $url — tried:\n  ${candidates.mkString("\n  ")}"
      ))
  }
}
