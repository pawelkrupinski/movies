package controllers

import play.api.mvc.RequestHeader

/**
 * Absolute base URL (`scheme://host`) for the current request, honouring
 * Fly's edge proxy.
 *
 * Behind Fly's edge, TLS terminates at the edge and the container
 * receives plain HTTP with `X-Forwarded-Proto: https` +
 * `X-Forwarded-Host`. We read those headers directly rather than
 * relying on Play's `play.http.forwarded.trustedProxies` machinery —
 * that didn't make `request.secure` reflect the proxied scheme on this
 * Play 3.0 setup. Trusting the headers is safe: Fly's edge is the only
 * ingress to our container, so the internet can't forge them. Falls
 * back to `request.secure` / `request.host` when the headers are absent
 * (local dev hitting localhost:9000 directly).
 */
object ForwardedUrl {
  def base(request: RequestHeader): String = {
    val scheme = request.headers.get("X-Forwarded-Proto").getOrElse(if (request.secure) "https" else "http")
    val host   = request.headers.get("X-Forwarded-Host").getOrElse(request.host)
    s"$scheme://$host"
  }
}
