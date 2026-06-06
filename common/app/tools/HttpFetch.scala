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
