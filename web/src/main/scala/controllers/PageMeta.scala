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

  /** `https://kinowo.net/?date=tomorrow` for a typical prod request.
   *  `canonicalUrl(request) == origin(request) + request.uri`.
   *
   *  Needs no knowledge of the deployment's mount point, even though three of
   *  the four now have one: `request.uri` is the path AS RECEIVED, and Play's
   *  `play.http.context` only affects which routes that path matches, not the
   *  RequestHeader — so `showtimes.cc/uk/kent/` canonicalises to itself. */
  def canonicalUrl(request: RequestHeader): String =
    origin(request) + request.uri

  /** `https://kinowo.net` — scheme + host without path/query.
   *  Reads `X-Forwarded-Proto` / `X-Forwarded-Host` directly — the
   *  `play.http.forwarded.trustedProxies` knob didn't make `request.secure`
   *  reflect the proxied scheme on this Play 3.0 setup (see the comment on
   *  `AuthController.callbackUrl`, which uses the same workaround). Safe
   *  because the Caddy vhost on k3s-worker-1 is the only ingress — the pod
   *  binds a NodePort the internet cannot reach, so these headers can't be
   *  forged by a client. */
  def origin(request: RequestHeader): String = {
    val proto = request.headers.get("X-Forwarded-Proto")
      .getOrElse(if (request.secure) "https" else "http")
    s"$proto://${host(request)}"
  }

  /** The public host this request arrived on — `kinowo.net`, or `showtimes.cc`
   *  whether it is the brand front door or a country served under it
   *  (`showtimes.cc/uk/…`). The proxy's `X-Forwarded-Host` wins over the
   *  container-local `Host`, which is what makes the front-door check work
   *  behind Caddy at all — though the host alone no longer decides it, since
   *  every Showtimes country's own pages arrive on that same host (see
   *  `models.Country.servesApex`). */
  def host(request: RequestHeader): String =
    request.headers.get("X-Forwarded-Host").getOrElse(request.host)

  /** `FB_APP_ID` is read once at boot — the value never changes per request,
   *  and the env layer (`tools.Env`) already falls back to `.env.local` so
   *  local dev can opt in without polluting the shell. `None` skips the
   *  `<meta property="fb:app_id">` emission entirely; production picks
   *  it up after registering an app at developers.facebook.com. */
  lazy val fbAppId: Option[String] = tools.Env.get("FB_APP_ID")
}
