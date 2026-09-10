package modules.wiring

import modules.WorkerWiring
import services.cinemas.common.ZyteFallback
import services.cinemas.pl.MultikinoClient
import services.cinemas.uk.OdeonAuthHarvester
import tools.{Env, FallbackHttpFetch, HostCircuitBreakerHttpFetch, HttpFetch, RealHttpFetch, ResidentialProxy, SessionWarmingHttpFetch, StickyShardHttpFetch}

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
  //
  // The proxy leg is circuit-broken (EgressWiring.breakerGuarded), unlike every
  // other leg of this chain: it is the only one built from raw RealHttpFetch
  // shards with none of HttpWiring's protective wrapping (no
  // HostCircuitBreakerHttpFetch, no per-host pacing). Without it, a Decodo
  // account-wide outage (every tunnel 503ing, 2026-09-10) makes EVERY venue call
  // pay the full connect/request timeout on a dead tunnel before FallbackHttpFetch
  // even tries the next leg — with only 4 worker threads, that alone starved
  // throughput across every task type sharing the pool, not just the proxied
  // scrapers, and surfaced as `Worker task queue head-of-line age high` (UK
  // climbed past 2600s). The breaker opens per destination host after a few
  // consecutive tunnel failures and fast-fails (~0ms) for the cooldown, so the
  // chain falls through to `fallback` almost immediately instead of queuing
  // behind a doomed proxy attempt on every single call.
  private def proxyPrimary(fallback: HttpFetch, warmUrl: Option[String] = None,
                           keyOf: String => String = StickyShardHttpFetch.hostAndPath): HttpFetch =
    proxyShards.fold(fallback) { shards =>
      val legs: IndexedSeq[HttpFetch] =
        warmUrl.fold[IndexedSeq[HttpFetch]](shards)(u => shards.map(new SessionWarmingHttpFetch(_, u)))
      val proxyLeg = EgressWiring.breakerGuarded(new StickyShardHttpFetch(legs, keyOf))
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
  // source (and, via flicksUs, the AMC/Regal/Malco fallback for the US), so the
  // block took all ~843 UK venues red at once.
  //
  // Residential proxy primary, Zyte fallback (added 2026-09-10, after the Decodo
  // account itself started 503ing every tunnel — `ProxyProbe` reproduced it from a
  // clean non-worker egress against api.ipify.org/Multikino/biletyna, so it is not
  // an IP-reputation block the direct leg would clear). Before this, the fallback
  // was plain `direct`, which is USELESS here: direct is the exact block the proxy
  // exists to clear, so a Decodo-side outage left Cineworld/Flicks/AMC/Regal/Malco
  // with no working path at all (8+ retries on a single UK Cineworld Leeds date
  // chunk). Zyte is billed per request, so this must stay BEHIND the proxy, never
  // primary — see feedback_zyte_is_decodo_fallback_only — and the
  // `kinowo-residential-proxy-failing` alert (now ResidentialProxyFallingBackToZyte,
  // routed to email) is what says whether that's actually happening.
  lazy val flicksFetch: HttpFetch = proxyPrimary(zyteFetch)

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
  // showtimes paths spread the sweep across the pool.
  //
  // Falls back to Zyte, not direct (changed 2026-09-10 alongside flicksFetch, same
  // Decodo-account-wide 503 outage — see the comment there). Odeon Middlesbrough hit
  // the identical "proxy: Tunnel failed, got: 503 / fallback: HTTP 403" loop with a
  // bare direct fallback, because direct is Cloudflare-blocked here too.
  lazy val odeonFetch: HttpFetch = proxyPrimary(zyteFetch)

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

  /** Wrap the sticky-shard proxy leg in a per-host circuit breaker, so a host
   *  whose Decodo tunnel starts failing (a pool-wide 503 spell, an account-level
   *  outage) stops paying the full connect/request budget on every call once it
   *  has opened. Extracted to a pure function — rather than inlined in
   *  `proxyPrimary` — so this composition is unit-testable without the rest of
   *  `WorkerWiring`. */
  private[wiring] def breakerGuarded(proxyLeg: HttpFetch): HttpFetch =
    new HostCircuitBreakerHttpFetch(proxyLeg)
}
