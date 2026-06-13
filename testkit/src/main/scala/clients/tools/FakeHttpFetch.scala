package clients.tools

import tools.{Env, HttpFetch}

import java.net.URI
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.CompletableFuture

class FakeHttpFetch(fixtureDir: String) extends HttpFetch {
  val fixtureRoot = FakeHttpFetch.rootFor(fixtureDir)

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
      .find(existsCaseExact)
      .map(p => Files.readAllBytes(p))
      .getOrElse(missingFixture(uri.getHost, path, url, candidates))
  }

  /** macOS APFS is case-insensitive, so `Files.exists` happily resolves a
   *  fixture requested with the wrong case (e.g. a client slug `"OPOLSKIELAMY"`
   *  against the recorded `opolskielamy.content`). Case-sensitive Linux CI
   *  404s on the same lookup, so the bug stays invisible until CI. Enforce
   *  case-exact matching here so a mis-cased URL fails the SAME way on a dev
   *  Mac as it does on CI: `toRealPath` reports the on-disk casing of every
   *  path component, which we compare component-by-component to the request. */
  private def existsCaseExact(p: Path): Boolean =
    Files.isRegularFile(p) && {
      val real = p.toRealPath()
      val want = p.toAbsolutePath.normalize
      real.getNameCount == want.getNameCount &&
        (0 until want.getNameCount).forall(i => real.getName(i).toString == want.getName(i).toString)
    }

  /** A missing **TMDB search** fixture means that query was simply never
   *  recorded — and in production an unknown query returns an empty result set,
   *  not an error. Mirror that: replay an empty TMDB search response so callers
   *  fall through to their next resolution strategy (originalTitle search,
   *  director-walk) deterministically, instead of the lookup throwing and the
   *  row's fate hinging on whether a sibling happened to resolve first (the
   *  PageSnapshot/e2e flake). Every other endpoint still throws — a missing
   *  detail / ratings / cinema-HTML fixture is a real recording gap. */
  private def missingFixture(host: String, path: String, url: String, candidates: Seq[String]): Array[Byte] =
    if (host == "api.themoviedb.org" && path.startsWith("3/search/"))
      """{"page":1,"results":[],"total_pages":1,"total_results":0}""".getBytes("UTF-8")
    else throw new java.io.FileNotFoundException(
      s"No fixture file for $url — tried:\n  ${candidates.mkString("\n  ")}")
}

object FakeHttpFetch {
  /** Fixture root for a dir: `test/resources/fixtures/<dir>` relative to the CWD
   *  (the repo root for sbt test/runMain), OR `<KINOWO_FIXTURE_ROOT>/<dir>` when
   *  that env/sysprop is set — so a process whose CWD is NOT the repo root (a
   *  forked `bgRunMain`, e.g. `sbt localStack`'s worker) can still find the
   *  corpus. Default (unset) is unchanged, so existing callers are unaffected. */
  def rootFor(fixtureDir: String): String = rootFor(fixtureDir, Env.get("KINOWO_FIXTURE_ROOT"))

  /** Pure form, for testing without touching the global env. */
  def rootFor(fixtureDir: String, base: Option[String]): String =
    base.filter(_.nonEmpty) match {
      case Some(b) => s"$b/$fixtureDir"
      case None    => "test/resources/fixtures/" + fixtureDir
    }
}
