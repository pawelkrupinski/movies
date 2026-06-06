package services.cinemas

import tools.GetOnlyHttpFetch

/**
 * Thin `HttpFetch` shim that routes GETs through `ZyteClient` so the caller
 * never has to know which proxy sits behind the `HttpFetch` it was given.
 *
 * `cookieSource` picks the fetch shape:
 *   - `Some(homepage)` → `getWithCookies` — warm a session from the homepage
 *     first, for upstreams with a session-cookie wall (Multikino).
 *   - `None` → a single `get`, for stateless pages that only need Zyte's
 *     residential egress to clear a datacenter-IP block (biletyna).
 */
class ZyteFetch(client: ZyteClient, cookieSource: Option[String]) extends GetOnlyHttpFetch {
  override def get(url: String): String =
    cookieSource.fold(client.get(url))(client.getWithCookies(url, _))
}
