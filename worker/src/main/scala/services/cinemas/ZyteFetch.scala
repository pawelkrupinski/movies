package services.cinemas

import tools.{Env, GetOnlyHttpFetch}

import java.time.Clock
import scala.concurrent.duration._

/**
 * Thin `HttpFetch` shim that routes GETs through `ZyteClient` so the caller
 * never has to know which proxy sits behind the `HttpFetch` it was given.
 *
 * `cookieSource` picks the fetch shape:
 *   - `Some(homepage)` → a [[SharedZyteSession]] — warm a session from the
 *     homepage once and reuse it across fetches, for upstreams with a
 *     session-cookie wall (Multikino). One `ZyteFetch` is shared across all the
 *     cinema clients, so the whole fleet shares one warmed session.
 *   - `None` → a single `get`, for stateless pages that only need Zyte's
 *     residential egress to clear a datacenter-IP block (biletyna).
 */
class ZyteFetch(
  client:       ZyteClient,
  cookieSource: Option[String],
  sessionTtl:   FiniteDuration = ZyteFetch.DefaultSessionTtl,
  clock:        Clock = Clock.systemUTC()
) extends GetOnlyHttpFetch {
  private val session: Option[SharedZyteSession] =
    cookieSource.map(src => new SharedZyteSession(client, src, sessionTtl, clock))

  override def get(url: String): String =
    session.fold(client.get(url))(_.get(url))
}

object ZyteFetch {
  /** How long a warmed Zyte session is reused before re-warming. Kept under a
   *  Zyte session's server-side lifetime so reuse usually hits a live session;
   *  if it's stale the fetch's 401 triggers a re-warm + retry anyway, so this is
   *  a cost knob, not a correctness one. Tunable via KINOWO_ZYTE_SESSION_TTL_SECONDS. */
  def DefaultSessionTtl: FiniteDuration = Env.positiveLong("KINOWO_ZYTE_SESSION_TTL_SECONDS", 480L).seconds
}
