package tools

import java.net.{InetSocketAddress, ProxySelector}
import java.nio.charset.StandardCharsets

/** Routes a `RealHttpFetch`'s outbound requests through an authenticated HTTP
 *  proxy — the Decodo static-residential (ISP) egress used for cinema sites
 *  that Cloudflare-block our Fly datacenter IP at the ASN level (Multikino,
 *  biletyna). The proxy IPs are genuine PL ISP (Netia, AS12741), so the target
 *  sees a residential egress and returns 200.
 *
 *  Holds the full egress pool — `ports` is one Decodo IP each. [[perPort]]
 *  yields a config pinned to each ([[pinnedTo]]); `StickyShardHttpFetch` fans a
 *  client's venues across those per-IP shards, keyed by venue URL so each venue
 *  sticks to one IP. Stickiness matters because Multikino's session cookie is
 *  bound to the egress IP — a venue's homepage-warm + API retry must share an
 *  IP (rotation warmed on one IP, retried on another → still 401, verified
 *  2026-06-16) — and each IP warms its own session once, reused across the
 *  venues routed to it. Spreading matters because funnelling all proxied auth
 *  through one IP tripped Decodo's "too many authentication attempts. Limit: 3"
 *  cap and rolled all proxied traffic to Zyte on 2026-06-16. See the
 *  `reference_decodo_isp_proxy` memory. */
case class ProxyConfig(host: String, ports: Seq[Int], user: tools.ProxyUser, password: tools.ProxyPassword) {
  require(ports.nonEmpty, "ProxyConfig needs at least one port")

  /** This config's sticky egress: the sole port of a [[pinnedTo]] config, or
   *  `ports.head` for the full pool (whose selector is unused — clients always
   *  pin first). */
  val port: Int = ports.head
  val selector: ProxySelector =
    ProxySelector.of(new InetSocketAddress(host, port))

  /** A copy pinned to a single egress IP from this pool — one shard's egress.
   *  Its own [[RealHttpFetch]] gives it its own cookie jar, so a sharded proxy
   *  can warm + reuse one Multikino session per IP while spreading venues across
   *  the pool rather than hammering one IP past Decodo's "Limit: 3" cap. */
  def pinnedTo(port: Int): ProxyConfig = {
    require(ports.contains(port), s"port $port is not one of the pool ports $ports")
    copy(ports = Seq(port))
  }

  /** One pinned config per pool port — the per-IP egresses a sharded proxy
   *  (see `StickyShardHttpFetch`) fans venues across. */
  def perPort: Seq[ProxyConfig] = ports.map(pinnedTo)

  /** The `Proxy-Authorization` value every request through this proxy carries. */
  def basicCredentials: String =
    "Basic " + java.util.Base64.getEncoder.encodeToString(s"${user.value}:${password.value}".getBytes(StandardCharsets.UTF_8))
}
