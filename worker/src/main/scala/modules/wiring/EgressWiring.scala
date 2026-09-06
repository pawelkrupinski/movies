package modules.wiring

import modules.WorkerWiring
import services.cinemas.common.ZyteFallback
import services.cinemas.pl.MultikinoClient
import services.cinemas.uk.OdeonAuthHarvester
import tools.{Env, FallbackHttpFetch, HttpFetch, RealHttpFetch, ResidentialProxy, SessionWarmingHttpFetch, StickyShardHttpFetch}

/** Cinema-site egress routes: the residential-proxy and Zyte chains the
 *  Cloudflare-blocked venues scrape through, each a seam the fixture wirings
 *  collapse back onto `httoFetch`. */
trait EgressWiring { self: WorkerWiring =>
  import EgressWiring.ResidentialProxyService

  // Residential-proxy egress (Decodo static-ISP, PL Netia) for the cinema sites
  // that Cloudflare-block our Fly datacenter IP. Non-secret host+ports come from
  // the committed residential-proxy.properties; the KINOWO_PROXY_USER/PASS secrets
  // come from Env (env -> .env.local). Some only when both are present — absent in
  // local/test, where the chain collapses to the existing Zyte/direct path. See
  // the `reference_decodo_isp_proxy` memory.
  // One RealHttpFetch per Decodo pool IP (each pinned, own cookie jar), built
  // once and shared by the proxied clients; None where the KINOWO_PROXY_* secrets
  // aren't set (local/CI/fixture-replay → Zyte/direct). Sharing the shards means
  // each IP warms its Multikino session at most once and reuses it across the
  // venues routed there.
  private lazy val proxyShards: Option[IndexedSeq[RealHttpFetch]] =
    ResidentialProxy.fromEnv().map(_.perPort.map(cfg => new RealHttpFetch(Some(cfg))).toIndexedSeq)

  // Proxy primary → existing chain (Zyte then direct) as fallback, so a proxy IP
  // that's ever unreachable/burned silently rolls over and scraping never breaks.
  //
  // StickyShardHttpFetch fans each client's venues across all the pool IPs, keyed
  // by venue URL so a given venue always egresses via the same IP while different
  // venues spread across the pool. This keeps Multikino off a single pinned IP —
  // notably it avoids getting stuck on `ports.head`, which is a M247 *datacenter*
  // IP that Multikino's Cloudflare blocks — and lets each IP hold its own warmed
  // Multikino session. (It is NOT what cleared the 2026-06-16 "Limit: 3" outage:
  // that was a Decodo auth rejection — the worker's egress IP fell off the
  // whitelist on a machine recreate — fixed account-side, not in this code.)
  //
  // `warmUrl` wraps EACH shard in its own SessionWarmingHttpFetch — for Multikino,
  // whose films API 401s on a cold call from the proxy IP until the homepage warms
  // a session cookie (verified 2026-06-16). Per-venue stickiness means the warm
  // and the API retry share an IP, and each IP warms once then reuses the cookie.
  // Stateless venues (biletyna, ck105) pass None.
  //
  // `keyOf` chooses the sticky-shard key. The default (host+path) spreads venues
  // across IPs — right for stateless per-venue scrapes (flicks, Cineworld). Vue
  // needs HOST-only stickiness instead: its token cookie is minted by a POST to
  // `/auth/token` and spent by a GET to `/…/films` — different PATHS — so host+path
  // would split them onto different IPs and lose the cookie. Host-only funnels all
  // of a brand's traffic onto one IP+cookie jar; fine at Vue's ~88-venue/420-min
  // volume, well under Decodo's concurrent-auth cap.
  private def proxyPrimary(fallback: HttpFetch, warmUrl: Option[String] = None,
                           keyOf: String => String = StickyShardHttpFetch.hostAndPath): HttpFetch =
    proxyShards.fold(fallback) { shards =>
      val legs: IndexedSeq[HttpFetch] =
        warmUrl.fold[IndexedSeq[HttpFetch]](shards)(u => shards.map(new SessionWarmingHttpFetch(_, u)))
      val proxyLeg = new StickyShardHttpFetch(legs, keyOf)
      new FallbackHttpFetch(Seq("proxy" -> proxyLeg, "fallback" -> fallback), onOutcome = recordProxyOutcome)
    }

  // Meter the residential-proxy leg to /uptime: a green "Residential proxy" bar
  // means the proxy served, a red one means it failed and we fell back to Zyte
  // (so red-bar frequency = how often Zyte is still used). Aggregated across all
  // proxied cinemas into one row. Only the outer chain's "proxy" leg is metered;
  // the inner Zyte/direct chain runs with the default no-op.
  private def recordProxyOutcome(backend: String, error: Option[String]): Unit =
    if (backend == "proxy") error match {
      case None        => uptimeMonitor.recordSuccess(ResidentialProxyService)
      case Some(label) => uptimeMonitor.recordFailure(ResidentialProxyService, label)
    }

  lazy val multikinoFetch: HttpFetch =
    proxyPrimary(MultikinoClient.fetchFor(httoFetch), warmUrl = Some(MultikinoClient.HomeUrl))
  // Zyte residential egress → direct fallback (Zyte only when ZYTE_API_KEY is set).
  lazy val zyteFetch: HttpFetch = ZyteFallback.fetchFor(httoFetch)
  // biletyna.pl 403s our datacenter IP; residential proxy primary, Zyte fallback.
  lazy val biletynaFetch: HttpFetch = proxyPrimary(zyteFetch)
  // www.flicks.co.uk 403s our datacenter IP behind Cloudflare (verified 2026-07-26
  // from kinowo-worker-uk: the identical GET returns 403 from Fly, 200 from a
  // residential IP; every Decodo pool IP returns 200 too). Flicks is the ONLY UK
  // source, so the block took all ~843 UK venues red at once. Residential proxy
  // primary, DIRECT fallback — no Zyte leg: flicks is a plain Cloudflare
  // IP-reputation block that the proxy clears, so paying Zyte per request across
  // 843 venues would buy nothing the proxy doesn't already give.
  lazy val flicksFetch: HttpFetch = proxyPrimary(httoFetch)

  // Vue/CinemaxX films API is Cloudflare-403'd from our Fly IP (like flicks) AND
  // token-gated, so it egresses residential AND host-sticky (one IP+cookie for the
  // token POST + films GET — see proxyPrimary/keyOf). Cineworld reuses flicksFetch
  // (GET-only, no cookie, so per-venue stickiness is fine). Both fall back to flicks
  // if the proxy is down. Showcase/Everyman still reach their origins directly.
  lazy val vueFetch: HttpFetch = proxyPrimary(httoFetch, keyOf = StickyShardHttpFetch.hostOnly)

  // vwc.odeon.co.uk — Odeon's Vista ocapi backend — is Cloudflare-403'd too. It was
  // NOT when the client was written: it answered our Fly egress directly, and only
  // the www page needed a browser. The 2026-08-29 move to Hetzner changed the egress
  // IP and the identical GET now returns 403 (a Cloudflare "Attention Required" page)
  // from k3s-worker-1 while returning 401 — i.e. reaching the origin, just
  // unauthenticated — from a residential IP and from every Decodo pool port. That
  // took all 102 Odeon venues red at once, so the data fetch moves onto the proxy.
  // Per-venue (default host+path) stickiness, not host-only: Odeon carries its auth
  // in a header, not a cookie, so nothing has to share an IP, and the per-date
  // showtimes paths spread the sweep across the pool. Falls back to direct — a
  // burned proxy is no worse than today, and the flicks fallback sits behind that.
  lazy val odeonFetch: HttpFetch = proxyPrimary(httoFetch)

  // Harvests Odeon's ~12h Vista JWT via Zyte browserHtml (the estate-wide token
  // lives in the Cloudflare-gated www page; the ocapi DATA host is open). Lazy TTL
  // cache — ~2 browser fetches/day — so Odeon's ocapi pulls run over plain `http`.
  // No key (CI/local) → token() is None → Odeon venues ride the flicks fallback.
  lazy val odeonAuthHarvester: OdeonAuthHarvester =
    new OdeonAuthHarvester(() => OdeonAuthHarvester.zyteFetchPage(Env.get("ZYTE_API_KEY")))
}

object EgressWiring {
  /** The /uptime row the residential-proxy leg is metered under. */
  private val ResidentialProxyService = "Residential proxy"
}
