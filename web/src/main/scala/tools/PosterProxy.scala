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

  // The `.poster-wrap` card slot is rendered with `padding-top: 148%`,
  // i.e. the slot is ~2:3 portrait. Asking weserv for a fixed 480×720
  // crop with `fit=cover&a=attention` gives us:
  //   - Landscape sources (banners / "_plakat_cut" crops some cinemas
  //     publish) are cropped to portrait around the salient region
  //     instead of being squashed by the browser's `object-fit: cover`
  //     center crop. weserv's `attention` algorithm (libvips smartcrop)
  //     picks the area with the highest visual entropy + face/skin
  //     hits — usually the actor's face on a movie poster.
  //   - Portrait sources at or near 2:3 pass through with minimal loss.
  //   - Bonus: the response is bounded at ~480×720 px regardless of
  //     source dimensions, so the megabyte-scale 4000×6000 poster
  //     masters some cinemas serve get trimmed at the proxy.
  private val TargetHeight = 720


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
  def proxy(url: String): String =
    weserv(url, TargetWidth, TargetHeight, "webp").getOrElse(url)

  /** A poster URL sized + re-encoded for the server-side OG-card compositor
   *  ([[OgCardService]]). Unlike [[proxy]] (webp, for the browser) this asks
   *  weserv for JPEG, because `javax.imageio` can't decode webp — a webp body
   *  would silently fail to read and drop the card to text-only. Targets a
   *  higher resolution than the browser card since the poster renders at up to
   *  ~520px tall on the 1200×630 card. SkipHosts (multikino) fall back to the
   *  origin URL, which already serves a JPEG `imageio` reads fine. */
  def posterForCard(url: String): String =
    weserv(url, 440, 660, "jpg").getOrElse(url)

  /** Build the weserv URL, or `None` for empty input / a [[SkipHosts]] origin
   *  the caller should fetch directly. weserv accepts the URL with or without
   *  the scheme prefix; stripping it avoids double-encoding `://` and lets the
   *  proxy pick the scheme (HTTPS when available, HTTP for origins like
   *  kinobulgarska19.pl). */
  private def weserv(url: String, w: Int, h: Int, output: String): Option[String] = {
    if (url == null || url.isEmpty) return None
    val stripped = url.replaceFirst("^https?://", "")
    val host     = stripped.takeWhile(_ != '/').toLowerCase
    if (SkipHosts.contains(host)) return None
    val encoded = URLEncoder.encode(stripped, "UTF-8")
    Some(s"https://images.weserv.nl/?url=$encoded&w=$w&h=$h&fit=cover&a=attention&output=$output")
  }
}
