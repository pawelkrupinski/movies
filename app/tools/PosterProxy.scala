package tools

import java.net.URLEncoder

/**
 * Wrap a cinema-side poster URL through the free `images.weserv.nl`
 * image proxy. Solves three concrete problems we measured on the
 * prod / page:
 *
 *   1. Mixed content. About 9 posters (`Kino Bułgarska 19`) ship as
 *      `http://kinobulgarska19.pl/...` and every modern browser
 *      silently blocks them on the HTTPS kinowo.fly.dev page —
 *      that's the primary cause of "the poster sometimes doesn't
 *      show". Routing through `https://images.weserv.nl/?url=...`
 *      makes the request HTTPS regardless of origin scheme.
 *
 *   2. Megabyte-scale full-res posters. Some cinema origins ship
 *      2-6 MB PNG/JPG masters for a card displayed at ~240 CSS-px
 *      wide. On cellular they routinely time out before the body
 *      finishes. `&w=480` resizes server-side; `&output=webp`
 *      re-encodes to webp (well supported since iOS 14, Chrome 32).
 *
 *   3. Ten different origin hosts (multikino.pl, kinoapollo.pl,
 *      cinema-city.pl, helios.pl, kinomuza.pl, kinopalacowe.pl,
 *      kinomalta.pl, kinobulgarska19.pl, bilety24.pl, plus TMDB).
 *      First page load needed a DNS + TLS handshake per host;
 *      unifying everything under `images.weserv.nl` collapses that
 *      to one warm connection.
 *
 * weserv.nl is HTTP-fetching protocol-aware: when the original is
 * HTTP-only (the kinobulgarska case) they still fetch it; when it's
 * HTTPS they prefer HTTPS. They cache aggressively on their own CDN,
 * so once the first user has triggered the proxy fetch every
 * subsequent visitor (and every CDN PoP) hits a warm cache.
 *
 * The `_movieCard` / `film.scala.html` `<img onerror>` already falls
 * back to a `.no-poster` placeholder if the proxy itself ever fails,
 * so weserv being a third-party dependency degrades to "no poster"
 * rather than a broken-image icon.
 */
object PosterProxy {
  // Card is ~240 CSS-px wide; bump to 480 to look crisp at DPR 2.
  // 480 is a reasonable cap — bigger uploads at the origin are just
  // wasted bytes on the wire.
  private val TargetWidth = 480

  // Hosts whose origin servers block weserv's outbound IP with 403
  // (cross-checked against direct-browser fetches that return 200).
  // For these, the proxy is a net regression — serve the original URL
  // and let the browser fetch it directly, where the request carries
  // a real UA + a `Referer: https://kinowo.fly.dev/` that the origin
  // accepts. Verified empirically on prod:
  //   probe of 178 unique poster URLs through weserv
  //     115  ✓ 200 image/webp
  //      63  ✗ 404 (weserv reported origin 403)
  //          → 62 of those were www.multikino.pl
  //          → 1 was a stale cinema-city URL (origin 404, scrape bug)
  // multikino origins are already HTTPS and serve ~150-300 KB JPEGs,
  // so skipping the proxy loses the webp/resize win but not the
  // mixed-content fix (there's no mixed content to fix) and not the
  // "actually displays" property the user reported.
  private val SkipHosts = Set(
    "www.multikino.pl",
    "multikino.pl"
  )

  /** Wrap a poster URL through weserv with width + format hints.
   *  Returns the original URL untouched when it's empty / null —
   *  callers shouldn't pass `None` in but the empty-string case can
   *  happen from `SourceData(posterUrl = Some(""))` corner cases. */
  def proxy(url: String): String = {
    if (url == null || url.isEmpty) return url
    // weserv accepts the URL with or without the scheme prefix. Strip
    // it so we don't double-encode `://` and so the proxy can pick
    // the right scheme automatically (HTTPS when available, falling
    // back to HTTP for HTTP-only origins like kinobulgarska19.pl).
    val stripped = url.replaceFirst("^https?://", "")
    val host = stripped.takeWhile(_ != '/').toLowerCase
    if (SkipHosts.contains(host)) return url
    val encoded  = URLEncoder.encode(stripped, "UTF-8")
    s"https://images.weserv.nl/?url=$encoded&w=$TargetWidth&output=webp"
  }
}
