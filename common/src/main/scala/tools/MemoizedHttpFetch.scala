package tools

import java.util.concurrent.ConcurrentHashMap
import scala.util.Try

/**
 * A short-lived decorator that fetches each URL at most once: the first `get`
 * for a URL runs the underlying fetch, and every later `get` for the SAME URL
 * replays that outcome — the body, or the exception it threw.
 *
 * Construct ONE per resolution attempt, never one per client. It has no expiry
 * and no size bound, so a long-lived instance would pin a stale body (and a
 * stale 404) forever. Its whole job is to remove the repeat fetches *inside a
 * single probe ladder*: the Metacritic and Rotten Tomatoes resolvers walk
 * several candidate titles whose slug lists overlap — a film whose original
 * title is "The Sting" and whose clean title is "Sting" probes `/movie/sting`
 * twice — and each repeat is a real round trip to the upstream.
 *
 * Failures are memoised deliberately. A 404 is the answer for a slug probe, not
 * an error to retry, and re-probing a slug that just 404'd inside the same
 * attempt cannot produce a different answer. Retrying across attempts is
 * unaffected: the next attempt builds a new instance.
 */
class MemoizedHttpFetch(underlying: HttpFetch) extends ForwardingHttpFetch {
  private val outcomes = new ConcurrentHashMap[String, Try[String]]()

  override def get(url: String): String =
    outcomes.computeIfAbsent(url, _ => Try(underlying.get(url))).get

  /** Every other member is forwarded, not memoised: headers can change what comes back (the URL alone is the wrong
   *  key for that), and no resolution ladder fetches bytes or posts. */
  protected def around[T](url: String)(call: HttpFetch => T): T = call(underlying)
}
