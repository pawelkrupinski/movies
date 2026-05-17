package services.cinemas

import tools.GetOnlyHttpFetch

/**
 * Thin `HttpFetch` shim that routes GETs through `ScrapingAntClient.getWithCookies`,
 * harvesting a session cookie from `homepage` first. Used at the composition
 * root to wrap a `ScrapingAntClient` so the caller (e.g. `MultikinoClient`)
 * never has to know whether ScrapingAnt is in the path or not — it just sees
 * an `HttpFetch`.
 *
 * The session-handling retry inside `MultikinoClient` becomes a no-op when
 * this wrapper is wired in: ScrapingAnt's first call succeeds (it does its
 * own cookie carryover) and the retry branch never fires.
 */
class ScrapingAntFetch(client: ScrapingAntClient, homepage: String) extends GetOnlyHttpFetch {
  override def get(url: String): String = client.getWithCookies(url, homepage)
}
