package services.cinemas.common

import tools.GetOnlyHttpFetch

/**
 * Thin `HttpFetch` shim that routes GETs through `ZyteClient` so the caller
 * never has to know which proxy sits behind the `HttpFetch` it was given. One
 * stateless extract call per fetch — for a page that only needs Zyte's
 * residential egress to clear an IP block (Kino Kryterium's ck105 portal).
 */
class ZyteFetch(client: ZyteClient) extends GetOnlyHttpFetch {
  override def get(url: String): String = client.get(url)

  /** The upstream's exact bytes — the inherited `get(url).getBytes(UTF_8)` would
   *  already have decoded a single-byte page as UTF-8 and mangled it. */
  override def getBytes(url: String): Array[Byte] = client.getBytes(url)

  /** Headers must reach the upstream — inheriting `HttpFetch`'s default
   *  (`get(url, headers) = get(url)`) silently dropped them, so a header-
   *  authenticated origin went out without its `Authorization` and paid for a 401. */
  override def get(url: String, headers: Map[String, String]): String =
    if (headers.isEmpty) get(url) else client.get(url, headers)
}
