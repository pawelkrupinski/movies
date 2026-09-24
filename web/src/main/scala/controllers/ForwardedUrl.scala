package controllers

import play.api.mvc.RequestHeader

/**
 * Absolute base URL (`scheme://host`) for the current request, honouring the
 * reverse proxy in front of the pods.
 *
 * Public traffic (through Cloudflare, for the proxied names) reaches Caddy on
 * the k3s host (`infra/nix/modules/roles/public-proxy.nix`), which terminates
 * TLS and `reverse_proxy`s plain HTTP to the web pods' NodePort, setting
 * `X-Forwarded-Proto: https` and `X-Forwarded-Host`. We read those headers
 * directly rather than relying on Play's `play.http.forwarded.trustedProxies`
 * machinery — that didn't make `request.secure` reflect the proxied scheme on
 * this Play 3.0 setup. Trusting the headers is safe: Caddy is the only public
 * ingress (the host firewall opens only 80/443 besides ssh), and it sets
 * the `X-Forwarded-*` headers itself, discarding any a client sent, since it
 * trusts no upstream proxy. Falls back to `request.secure` / `request.host`
 * when the headers are absent (local dev hitting localhost:9000 directly).
 */
object ForwardedUrl {
  def base(request: RequestHeader): String = {
    val scheme = request.headers.get("X-Forwarded-Proto").getOrElse(if (request.secure) "https" else "http")
    val host   = request.headers.get("X-Forwarded-Host").getOrElse(request.host)
    s"$scheme://$host"
  }

  /** `scheme://host[:port]` of an absolute URL — what `base` answers for the
   *  request that URL names. */
  def originOf(url: String): String = url.split('/').take(3).mkString("/")
}
