package tools

/** The residential proxy's egress as fetches: one [[RealHttpFetch]] per Decodo pool IP, each pinned
 *  with its own cookie jar — or None when the proxy isn't configured (no `DECODO_PROXY_*` secrets:
 *  local, CI, fixture replay). Shared by the worker's egress wiring and the tools that reach the
 *  same Cloudflare-blocked sites, so neither builds the shards its own way. */
object ResidentialShards {
  def apply(config: Option[RealHttpFetch.ProxyConfig], tls: javax.net.ssl.SSLContext): Option[IndexedSeq[RealHttpFetch]] =
    config.map(_.perPort.map(cfg => new RealHttpFetch(Some(cfg), tls)).toIndexedSeq)

  /** The shards of the proxy `configuration` names. */
  def fromConfiguration(configuration: settings.ProcessConfiguration, tls: javax.net.ssl.SSLContext): Option[IndexedSeq[RealHttpFetch]] =
    ResidentialShards(ResidentialProxy.fromConfiguration(configuration), tls)
}
