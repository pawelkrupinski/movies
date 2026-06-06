package services.cinemas

import tools.GetOnlyHttpFetch

/**
 * Thin `HttpFetch` shim that routes GETs through `ZyteClient.getWithCookies`
 * so the caller (`MultikinoClient`) never has to know which proxy sits
 * behind the `HttpFetch` it was given.
 */
class ZyteFetch(client: ZyteClient, homepage: String) extends GetOnlyHttpFetch {
  override def get(url: String): String = client.getWithCookies(url, homepage)
}
