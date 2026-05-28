package controllers

import play.api.mvc.RequestHeader

/** Shared helpers for the `<meta>` block on every server-rendered page —
 *  notably the OG `pageUrl` and the optional `fb:app_id`. Centralised here
 *  so any controller that hands data to a Twirl template can pull the
 *  same canonical-URL + FB-app-id pair without re-deriving each.
 *
 *  Why these matter for share previews: Facebook's debugger flags
 *  `og:url` and `fb:app_id` as missing required properties when either is
 *  absent. `og:url` also affects Messenger's link-preview card — its
 *  scraper reads from the same FB cache and occasionally drops cards whose
 *  scrape returned no canonical URL. `fb:app_id` is informational (only
 *  unlocks domain insights), so we surface it via env var so prod can opt
 *  in once a Facebook App is registered without forcing test rigs / dev
 *  to invent fake IDs.
 */
object PageMeta {

  /** `https://kinowo.fly.dev/?date=tomorrow` for a typical prod request.
   *  `canonicalUrl(request) == origin(request) + request.uri`. */
  def canonicalUrl(request: RequestHeader): String =
    origin(request) + request.uri

  /** `https://kinowo.fly.dev` — scheme + host without path/query.
   *  Reads `X-Forwarded-Proto` / `X-Forwarded-Host` directly — the
   *  `play.http.forwarded.trustedProxies` knob didn't make `request.secure`
   *  reflect the proxied scheme on this Play 3.0 setup (see the comment on
   *  `AuthController.callbackUrl`, which uses the same workaround). Safe
   *  because Fly's edge is the only ingress; the internet can't reach our
   *  container to forge these headers. */
  def origin(request: RequestHeader): String = {
    val proto = request.headers.get("X-Forwarded-Proto")
      .getOrElse(if (request.secure) "https" else "http")
    val host  = request.headers.get("X-Forwarded-Host").getOrElse(request.host)
    s"$proto://$host"
  }

  /** `FB_APP_ID` is read once at boot — the value never changes per request,
   *  and the env layer (`tools.Env`) already falls back to `.env.local` so
   *  local dev can opt in without polluting the shell. `None` skips the
   *  `<meta property="fb:app_id">` emission entirely; production picks
   *  it up after registering an app at developers.facebook.com. */
  lazy val fbAppId: Option[String] = tools.Env.get("FB_APP_ID")
}
