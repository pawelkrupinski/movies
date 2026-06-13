package tools

import java.util.concurrent.CompletableFuture

/**
 * Generic HTTP fetch surface used by every external-API client in the
 * project. Implementations are wired at the composition root (`AppLoader`
 * for production via `RealHttpFetch`, per-spec via fixtures in tests).
 *
 * Both `get` and `post` are part of the contract: any implementation must
 * support both. Test fakes that only ever exercise GET-driven code paths
 * can extend [[GetOnlyHttpFetch]], which makes the "I don't support POST"
 * intent explicit by failing loudly if a caller ever tries.
 */
trait HttpFetch {
  def get(url: String): String

  /** Fetch the raw response bytes, undecoded. The default re-encodes the
   *  UTF-8 string `get` returns, which is lossless only for genuinely UTF-8
   *  upstreams — the vast majority. Callers scraping a legacy single-byte
   *  site that ships NO charset declaration (Kino Charlie serves raw
   *  ISO-8859-2 with `Content-Type: text/html` and no charset) need the true
   *  bytes so they can decode with the right charset: a UTF-8 decode of those
   *  bytes is lossy (~580 `U+FFFD` on Charlie's page), so the String `get`
   *  returns can't be round-tripped back. `RealHttpFetch` overrides this to
   *  return the wire bytes and `FakeHttpFetch` returns the fixture file's
   *  bytes. IMPORTANT: any DELEGATING wrapper (MonitoringHttpFetch,
   *  CachingDetailFetch, MongoCachingDetailFetch, FallbackHttpFetch) MUST
   *  override this to forward to its underlying's `getBytes` — inheriting this
   *  default round-trips the wire bytes through a UTF-8 decode and mojibakes a
   *  legacy single-byte page ("ż" → "ďż˝"). The default is only safe for a
   *  leaf String-only fetch (e.g. a GET-only test fake). */
  def getBytes(url: String): Array[Byte] =
    get(url).getBytes(java.nio.charset.StandardCharsets.UTF_8)

  /** GET with extra request headers. Default delegates to the no-header
   *  form, which lets test fakes (FakeHttpFetch, RecordingHttpFetch) keep
   *  overriding only `get(url)` — they fingerprint by URL and don't care
   *  about headers. Production callers that need per-call auth (e.g.
   *  TMDB v4 Bearer) use this overload; `RealHttpFetch` overrides it to
   *  pass headers through. */
  def get(url: String, headers: Map[String, String]): String = get(url)

  def getAsync(url: String): CompletableFuture[String] =
    CompletableFuture.supplyAsync(() => get(url))

  def post(url: String, body: String, contentType: String = "application/json"): String
}

/**
 * Test convenience: extend this when the unit under test never calls
 * `post`. Makes the assumption explicit at the type level and crashes
 * loudly if it ever stops holding — better than the silent surprise of a
 * mistakenly-shared base type whose `post` returns `""` or hangs.
 */
trait GetOnlyHttpFetch extends HttpFetch {
  override def post(url: String, body: String, contentType: String): String =
    throw new UnsupportedOperationException(
      s"GetOnlyHttpFetch does not implement post (url=$url)"
    )
}
