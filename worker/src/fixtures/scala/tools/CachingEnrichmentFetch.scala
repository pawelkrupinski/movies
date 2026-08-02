package tools

import scala.util.control.NonFatal

/**
 * The enrichment fetch the country convergence specs run on: an [[HttpFetch]]
 * that answers from the country's [[EnrichmentCache]] first and only then reaches
 * the live service, remembering whatever came back — INCLUDING the failure.
 *
 * Why the convergence suite needs one at all. The suite's whole claim is that the
 * pipeline reaches a fixpoint: settle twice, tick twice, and nothing moves. Every
 * enrichment field was excluded from that claim by running the whole replay on a
 * fetch that refused every call, because a live lookup answers on its own schedule and a
 * corpus whose ratings shifted between pass 1 and pass 2 would fail the fixpoint
 * for a reason that has nothing to do with the pipeline. A cache removes exactly
 * that objection: the first pass fills it, every later pass replays it
 * byte-for-byte, and the enrichment fields become as deterministic as the scraped
 * ones — so they can join the equality check instead of sitting it out as `None`.
 *
 * Failures are cached for the same reason successes are, and it matters more. A
 * country corpus is full of titles TMDB will never match and slugs Metacritic
 * doesn't have; if only successes were remembered, every pass would re-ask the
 * live service for every one of those misses — the slowest part of the sweep and
 * the part whose answer is least stable. A remembered 404 is a remembered verdict.
 *
 * A cached failure is REVIVED, not merely reported: an [[HttpStatusException]]
 * comes back as an `HttpStatusException` carrying its original status, because
 * callers branch on the code (a 404 is a permanent miss, a 5xx is worth a retry)
 * and a replay that flattened them into one exception type would make pass 2
 * behave differently from pass 1 — the precise thing this exists to prevent.
 *
 * Keys are [[RedactedUrl]]-masked, so the credential TMDB and OMDb put in the
 * query string never reaches the collection. Masking replaces only the VALUE and
 * preserves the URL's shape, so two genuinely different requests stay two
 * different keys.
 */
class CachingEnrichmentFetch(cache: EnrichmentCache, underlying: HttpFetch) extends HttpFetch {

  override def get(url: String): String =
    cached(CachingEnrichmentFetch.keyOf("GET", url), "GET", url)(
      CachingEnrichmentFetch.asBody)(CachedResponse.Body.apply)(underlying.get(url))

  // Every enrichment client sends a constant header set (a user agent, or the
  // TMDB v4 bearer that is an alternative spelling of the api_key already in the
  // key), so headers don't discriminate between two responses. Same key as the
  // bare form, exactly as `MongoCachingDetailFetch` treats its detail pages.
  override def get(url: String, headers: Map[String, String]): String =
    cached(CachingEnrichmentFetch.keyOf("GET", url), "GET", url)(
      CachingEnrichmentFetch.asBody)(CachedResponse.Body.apply)(underlying.get(url, headers))

  // Cached as base64 rather than left to the inherited default, which re-encodes
  // the String form as UTF-8 and mojibakes any legacy single-byte page (see the
  // warning on `HttpFetch.getBytes`). Kept on its own key so a body and its bytes
  // can never be served for one another.
  override def getBytes(url: String): Array[Byte] =
    cached(CachingEnrichmentFetch.keyOf("BYTES", url), "GET", url)(
      { case CachedResponse.Bytes(base64) => java.util.Base64.getDecoder.decode(base64) })(
      bytes => CachedResponse.Bytes(java.util.Base64.getEncoder.encodeToString(bytes)))(
      underlying.getBytes(url))

  // POST is the IMDb GraphQL rating query, whose whole request lives in the body —
  // so the body is part of the key, or every film would share one cached answer.
  override def post(url: String, body: String, contentType: String): String =
    cached(CachingEnrichmentFetch.keyOf("POST", url, Some(body)), "POST", url)(
      CachingEnrichmentFetch.asBody)(CachedResponse.Body.apply)(underlying.post(url, body, contentType))

  /**
   * Answer from the cache, or fill it from `fetch` — the one path every verb takes.
   *
   * `decode` says which remembered shape counts as an answer for this verb (a body
   * for `get`/`post`, bytes for `getBytes`), so a key can never be served as the
   * wrong kind; `encode` says how the fetched value is remembered.
   */
  private def cached[A](key: String, method: String, url: String)
                       (decode: PartialFunction[CachedResponse, A])
                       (encode: A => CachedResponse)(fetch: => A): A = {
    def hit: Option[A] = replay(key, url).collect(decode)

    hit.getOrElse {
      cache.singleFlight(key) {
        // Re-checked under the lock: a concurrent pass may have filled this key
        // while we queued, and taking its answer is the whole point of the lock.
        hit.getOrElse {
          try {
            val fetched = fetch
            cache.remember(key, encode(fetched))
            fetched
          } catch {
            case NonFatal(failure) =>
              cache.remember(key, CachingEnrichmentFetch.failureOf(failure, method))
              throw failure
          }
        }
      }
    }
  }

  /** A remembered FAILURE is thrown from here rather than returned, so the caller
   *  sees pass 2 fail exactly the way pass 1 did. */
  private def replay(key: String, url: String): Option[CachedResponse] =
    cache.lookup(key) match {
      case Some(failed: CachedResponse.Failed) => throw CachingEnrichmentFetch.revive(failed, url)
      case other                               => other
    }
}

object CachingEnrichmentFetch {

  /** The remembered shape a String-returning verb accepts as its answer. */
  private val asBody: PartialFunction[CachedResponse, String] = { case CachedResponse.Body(text) => text }

  /** `METHOD <credential-masked url>`, plus a body fingerprint where the body is
   *  what distinguishes two calls. */
  def keyOf(method: String, url: String, body: Option[String] = None): String = {
    val masked = RedactedUrl(url)
    body match {
      case None       => s"$method $masked"
      case Some(text) => s"$method $masked ${Integer.toHexString(text.hashCode)}"
    }
  }

  /** Reconstruct the exception a cached failure stands for. A status-bearing
   *  failure comes back as the same typed [[HttpStatusException]] with the same
   *  code — `retryAfter` deliberately dropped, since a cached 429's original hint
   *  expired with the run that saw it. */
  def revive(failed: CachedResponse.Failed, url: String): RuntimeException = failed.status match {
    case Some(code) => new RememberedHttpStatusException(code, failed.method, url)
    case None       => new CachedEnrichmentFailure(failed.message)
  }

  /** What to remember about a failure. Status-bearing failures keep their code;
   *  everything else (a timeout, a reset socket) keeps its class name in the
   *  message so a puzzling cached miss can still be diagnosed a day later. */
  def failureOf(failure: Throwable, method: String): CachedResponse.Failed = failure match {
    case status: HttpStatusException =>
      CachedResponse.Failed(Some(status.code), status.method, status.getMessage)
    case other =>
      CachedResponse.Failed(None, method, s"${other.getClass.getName}: ${other.getMessage}")
  }
}

/** A replayed non-HTTP failure — a timeout or connection error the cache saw on an
 *  earlier run. Carries the original's message; the type is deliberately distinct
 *  so a confusing test failure names the cache rather than pretending to be a live
 *  socket error. */
class CachedEnrichmentFailure(message: String) extends RuntimeException(message)

/**
 * A replayed HTTP failure, saying so.
 *
 * Still an [[HttpStatusException]] carrying the original code, because callers branch
 * on it — a 404 is a permanent miss, a 5xx is worth a retry — and pass 2 has to behave
 * exactly as pass 1 did. Only the MESSAGE differs, and that matters more than it
 * sounds: a remembered 404 and a live one were textually identical in the log, so a
 * fully-cached run making no network calls whatsoever was indistinguishable from one
 * hammering Rotten Tomatoes on every film. That ambiguity is what made "is the 404
 * actually cached?" impossible to answer without correlating cache statistics.
 */
class RememberedHttpStatusException(code: Int, method: String, url: String)
  extends HttpStatusException(code, method, url, retryAfter = None) {
  override def getMessage: String = s"${super.getMessage} (remembered — not re-fetched)"
}
