package services.cinemas

import tools.GetOnlyHttpFetch

/**
 * Mirror of `ScrapingAntFetch` for Zyte. Hands every GET to
 * `ZyteClient.getWithCookies` so the caller (`MultikinoClient`) never
 * has to know which proxy SaaS sits behind the `HttpFetch` it was
 * given.
 */
class ZyteFetch(client: ZyteClient, homepage: String) extends GetOnlyHttpFetch {
  override def get(url: String): String = client.getWithCookies(url, homepage)
}
