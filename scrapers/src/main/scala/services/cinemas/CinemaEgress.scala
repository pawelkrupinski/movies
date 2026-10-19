package services.cinemas

import services.cinemas.pl.MultikinoClient
import tools.{HttpFetch, ProxyChain, Stopwatch}

import java.time.Clock

/**
 * The routes the catalog's venues fetch through, one per kind of venue whose origin refuses the
 * worker's own egress. `WorkerWiring` routes each through its proxy or Zyte leg; the diagnostic
 * catalog ([[CinemaEgress.diagnostic]]) through the residential proxy shards it is handed; the
 * fixture wirings and specs back to one fetch ([[CinemaEgress.direct]]).
 *
 * @param direct    the shared `HttpFetch` every venue without a route of its own fetches through.
 * @param multikino Multikino's API: residential proxy → direct in production.
 * @param biletyna  biletyna.pl's venue pages, which 403 a datacenter IP (Cloudflare waiting-room).
 * @param zyte      Zyte residential egress for venues whose origin firewall blocks BOTH our datacenter
 *                  IP AND the Decodo proxy's (datacenter-flavoured) ISP IPs: Kino Kryterium /
 *                  bilety.ck105.koszalin.pl times out the connection from both, and only Zyte's
 *                  true-residential network gets through.
 * @param flicks    Decodo residential egress for flicks.co.uk, which Cloudflare 403s from our
 *                  datacenter IP. Flicks is most UK venues' source (and every one's fallback), so a
 *                  direct fetch blacks out the whole country (2026-07-26).
 * @param vue       host-sticky residential egress for Vue/CinemaxX: their films API is Cloudflare-403'd
 *                  from our IP AND token-cookie-gated, so it needs a residential IP that STAYS the same
 *                  across the token POST + films GET — `flicks` is per-venue sticky, which would split them.
 * @param odeon     residential egress for Odeon's Vista ocapi backend, which 403s the Hetzner egress the
 *                  worker moved to on 2026-08-29. Its own route so an Odeon venue keeps its flicks
 *                  FALLBACK on the flicks route while its own-site primary rides this one.
 * @param odeonAuthToken Odeon's short-lived Vista JWT (harvested via Zyte in production, see
 *                  [[services.cinemas.uk.OdeonAuthHarvester]]); without one an Odeon client throws, and
 *                  its venue falls back to flicks.
 */
final case class CinemaEgress(
  direct:         HttpFetch,
  multikino:      HttpFetch,
  biletyna:       HttpFetch,
  zyte:           HttpFetch,
  flicks:         HttpFetch,
  vue:            HttpFetch,
  odeon:          HttpFetch,
  odeonAuthToken: () => Option[String]
)

object CinemaEgress {

  /** Every route `http`, and no Odeon token: a fixture wiring's or a spec's. */
  def direct(http: HttpFetch): CinemaEgress = CinemaEgress(http, http, http, http, http, http, http, () => None)

  /** A diagnostic's routes (`FilmwebDiff`, `RosterAudit`, specs): every paid route — Multikino's API,
   *  biletyna's venue pages, the venue behind `zyte` — is the residential proxy over `proxyShards`
   *  first, then `http`; with no shards it is `http` ALONE
   *  ([[ProxyChain.paidEgressChain]]). A diagnostic never builds a Zyte leg, nor wires
   *  a token harvester (so Odeon venues throw → flicks fallback). The UK routes stay on `http`: Decodo's
   *  IPs are Polish, and a diagnostic runs Poland. `clock` is what the proxy leg's circuit breaker is
   *  judged on — the run's own, which a venue clock pinned to a day for the plan's sake is not. A diagnostic's
   *  elapsed time is the process's own monotonic [[Stopwatch.System]]: this shape is the catalog's diagnostic door,
   *  which the identity tools compile into older refs, so it takes no stopwatch of its own. */
  def diagnostic(http: HttpFetch, clock: Clock, proxyShards: Option[IndexedSeq[HttpFetch]]): CinemaEgress = {
    val paidRoute = ProxyChain.paidEgressChain(proxyShards, http, clock, Stopwatch.System)
    direct(http).copy(multikino = multikinoChain(proxyShards, http, clock, Stopwatch.System),
      biletyna = paidRoute, zyte = paidRoute)
  }

  /** Multikino's chain for a recording or diagnostic tool: proxy (warmed on the homepage) → `direct`. */
  def multikinoChain(proxyShards: Option[IndexedSeq[HttpFetch]], direct: HttpFetch, clock: Clock, stopwatch: Stopwatch): HttpFetch =
    ProxyChain.paidEgressChain(proxyShards, direct, clock, stopwatch, Some(MultikinoClient.HomeUrl))
}
