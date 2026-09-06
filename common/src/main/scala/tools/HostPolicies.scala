package tools

import java.net.URI
import java.time.Duration

/** A per-host override of the network timeouts. `connectTimeout` bounds the
 *  TCP+TLS handshake (the JVM throws HttpConnectTimeoutException if either runs
 *  over); `requestTimeout` bounds the response read. Each defaults to the
 *  matching [[HostPolicies]] default, so a row overrides only what it names.
 *  `hostSuffixes` matches by exact host or a dotted sub-domain (see
 *  `HostPolicies.hostMatches`), so `www.x` matches `x` but an unrelated `*.y`
 *  does not.
 *
 *  `minRequestInterval` is the OUTBOUND pace [[RateLimitedHttpFetch]] holds the
 *  whole fleet to for this host — the minimum gap between two requests to it,
 *  across every thread. `None` (the default) means unpaced: the host absorbs
 *  our natural concurrency, so only a host we structurally out-run names one.
 *
 *  `paceKnob` names an [[Env]] key that overrides `minRequestInterval` at
 *  RUNTIME. A host whose tolerance we don't know (nobody publishes one) can
 *  only be tuned empirically — push the pace until the 429s stop — and doing
 *  that through redeploys costs a worker restart and a cold JVM per attempt.
 *  Naming a knob makes the pace flippable from `/admin/config` mid-flight: the
 *  resolve in `HostPolicies` reads it per request, so the next request uses the
 *  new value. Still DATA, not an if-branch — any future host can name its own key.
 *
 *  `headers` are sent on EVERY request to this host, GET and POST alike, on top
 *  of the defaults — for a host whose edge admits us only when we identify
 *  ourselves a particular way. An explicit caller header of the same name still
 *  wins (per-call beats per-host). This belongs on the transport, not the client,
 *  because it is a property of the ORIGIN's admission rule rather than of the API
 *  contract — and because applying it at the terminal fetch means none of the
 *  eleven `HttpFetch` decorators in the chain can silently drop it. */
final case class HostPolicy(
  hostSuffixes: Set[String],
  connectTimeout: Duration = HostPolicies.DefaultConnectTimeout,
  requestTimeout: Duration = HostPolicies.DefaultRequestTimeout,
  minRequestInterval: Option[Duration] = None,
  paceKnob: Option[String] = None,
  headers: Map[String, String] = Map.empty,
)

/** The per-host network policy [[RealHttpFetch]] consults — the ONE place a host
 *  earns a non-default connect/request timeout, an outbound pace, or an
 *  identifying header. A host that needs any of those gets ONE row in [[all]]
 *  — no bespoke predicate, constant, or if-branch per host. The lookups below
 *  all read that one table (first matching row wins). Add a host = append a
 *  row; nothing else changes.
 *
 *  Rows match by host SUFFIX, so a sibling market on a different host (a new
 *  Flicks or Webedia ccTLD) gets NOTHING from its sibling's row — it is unpaced
 *  until it earns its own.
 *
 *  This is a plain `object`, initialised in declaration order on first touch:
 *  the defaults come before the table that reads them, and `RealHttpFetch`'s
 *  eager per-instance client build reads [[all]] — so the whole table exists
 *  before any client does. Keep every member a strict `val`; a lazy row could
 *  reorder that. */
object HostPolicies {

  /** The tight default connect budget: a live host's TCP+TLS handshake finishes
   *  in well under a second, so 5s frees a stalled fan-out slot fast. See the
   *  comment on RealHttpFetch's client builder for why low matters. */
  val DefaultConnectTimeout: Duration = Duration.ofSeconds(5)

  /** The default per-request (response-read) budget: a slow-but-alive upstream
   *  legitimately needs longer than the connect phase, so this stays generous. */
  val DefaultRequestTimeout: Duration = Duration.ofSeconds(30)

  /** The per-host policy table — the single place a host earns a non-default
   *  timeout, pace, or header. First matching row wins. */
  val all: Seq[HostPolicy] = Seq(
    // Helios's REST API (restapi.helios.pl) — both the screening/event LIST that
    // HeliosClient fetches per cinema AND the per-screen/detail enrichment. On
    // 2026-07-05 the host degraded to answering in ~5-7s: UNDER the old 8s budget,
    // so calls SUCCEEDED (slow) instead of timing out — the HostCircuitBreaker only
    // trips on timeouts/5xx, so it never opened, and ~30 Helios venues each holding
    // a fetch ~5-7s pinned the worker's CPU and drained shared-cpu credit to the
    // floor for hours. 4s is above the ~1-3s a healthy call takes but BELOW the slow
    // tail, so a degraded host now TIMES OUT → the breaker opens → Helios is skipped
    // for the cooldown (it degrades to its NUXT repertoire; detail only enriches).
    HostPolicy(Set("restapi.helios.pl"), requestTimeout = Duration.ofSeconds(4)),

    // Metacritic's slow Cloudflare-fronted origin. It answers our datacenter
    // egress in ~1-6s on a good day (≈35% of metascore fetches legitimately take
    // >5s, so the 8s budget above would cut them), but a Cloudflare challenge or
    // hang can stretch a page past the 30s default and pin a rating-refresh slot.
    // 15s frees the slot ~2× sooner while clearing the healthy tail; metascore
    // enrichment is best-effort, retried next cycle, so an occasional cut is free.
    HostPolicy(Set("metacritic.com"), requestTimeout = Duration.ofSeconds(15)),

    // Kino Iluzjon (Filmoteka Narodowa). Its TLS handshake is pathologically slow
    // — the TCP connect lands instantly but the handshake itself takes 20-30s (the
    // server's own latency, reproducible with openssl, not our cert path). Under
    // the 5s default connect budget every fetch died with HttpConnectTimeout-
    // Exception, leaving it perpetually red on /uptime though the page returns 200
    // given time. 40s covers the handshake; the read budget stays the default.
    HostPolicy(Set("iluzjon.fn.org.pl"), connectTimeout = Duration.ofSeconds(40)),

    // IMDb's GraphQL CDN — the endpoint every IMDb rating and the whole
    // SourceData(Imdb) slot (director, cast, runtime, poster) comes from. On
    // 2026-08-01 its edge began 403-ing any request that doesn't name a client:
    // an nginx "403 Forbidden" HTML page, so the block is at the CDN, before the
    // API. Nothing about us changed and it is not an IP ban — a residential IP is
    // refused identically, and the Fly egress is served identically once the
    // header is present. The symptom was silent: `ImdbClient.lookup` swallows the
    // throw into `None`, so every film in all three countries logged "rating none"
    // while the ratings simply stopped refreshing. `imdb-web-next` is the name
    // imdb.com's own front-end sends. The suggestion host (v3.sg.media-imdb.com,
    // used by `findId`) is a different origin and is NOT blocked — no row for it.
    HostPolicy(
      Set("caching.graphql.imdb.com"),
      headers = Map("x-imdb-client-name" -> "imdb-web-next"),
    ),

    // Filmstarts (Webedia DE). Germany's 1,529 venues each fan out one listing
    // fetch plus one request per advertised day onto ONE origin, and with no
    // pacing the worker's fan-out delivers them in bursts the host answers with
    // 429. That was our steady state, not an anomaly: ThrottledHttpFetch's
    // reactive 5s gate then parked the venue past AdaptiveTimeoutScraper's
    // budget, so the scrape was cut and DE went stale. 250ms (~4 req/s) was that
    // starting pace, and the 429s never stopped: one live worker.log carried
    // 3,118 of them, and 429 was the ONLY HTTP status in it — no 403s, no 5xx,
    // so this is purely self-inflicted, not a Fly-ASN block.
    // The cost is worse than the lost request: a 429 trips
    // HostCircuitBreakerHttpFetch, whose 60s open window fast-fails all of a
    // venue's day requests at once ("all 7 showtime requests ... failed"), and
    // RetryWithBackoff then re-runs the whole cinema up to 3x. So the fast pace
    // spent its budget three times over on requests that could never land, and
    // only ~30% of scrapes were succeeding.
    // The pace-report logging (see ThrottledHttpFetch, surfaced at INFO in the
    // worker's logback-base.xml) turned that search into measurement. 500ms was
    // 100% clean overnight but only ~95% under German morning load — a steady
    // ~4-5% throttle with bursts to ~35%. 1000ms (~1 req/s) looked clean at the
    // time, but panel-14 of kinowo-worker-diag over 2026-07-27/28 showed it was
    // not: a metronome-flat 60 req/min for ~15min, then 4 consecutive 429s, then
    // a ~5min DE-wide scrape blackout, on a ~20min cycle. The blackout is the
    // breaker doing its job — during it we sent 1-4 req/min and Filmstarts 429'd
    // EVERY one, so it blocks us outright for minutes once tripped — but it cost
    // ~25% of DE's scrape wall-clock and 9,648 rescheduled ScrapeChunk tasks a
    // day. 1 req/s simply sits above what Filmstarts tolerates sustained: the
    // long-run rate we actually got through, blackouts included, was ~45 req/min.
    // 1400ms (~43 req/min) drops just under that ceiling for 0 throttled.
    // The cost is the sweep, but less than the old ×7 arithmetic suggested: the
    // client reads `data-showtimes-dates` and fetches only ADVERTISED days
    // (production counters: 51,973 day-chunks over 14,862 venue sweeps = ~3.5
    // days/venue), so a sweep is ~6.9k requests, not 10.7k — ~161min at 1400ms,
    // still inside DE's cadence (its k3s overlay), which therefore does
    // NOT move with this. Pace and cadence stay coupled all the same — pace sets
    // sweep length, cadence sets the budget, and WorkerScrapeCadenceConfigSpec
    // asserts sweep ≤ cadence so neither can drift alone.
    // KINOWO_FILMSTARTS_PACE_MS still overrides this live (per request) for
    // re-tuning without a redeploy.
    HostPolicy(
      Set("filmstarts.de"),
      minRequestInterval = Some(Duration.ofMillis(1400)),
      paceKnob           = Some("KINOWO_FILMSTARTS_PACE_MS"),
    ),

    // SensaCine (Webedia ES). Spain's 594 venues reach the SAME client Germany
    // does on a DIFFERENT host, and that difference is the whole reason this row
    // exists: `HostPolicies` rows match by host SUFFIX, so `filmstarts.de` does
    // NOT match `www.sensacine.com` and a market without its own row is not paced
    // AT ALL. That is the exact condition that produced the UK's self-inflicted
    // 429 storm, and it would be worse here because the two markets look
    // identical in the code — one client, one parser, one set of dashboards.
    //
    // 1400ms is Filmstarts' number, adopted deliberately rather than measured:
    // same vendor, same platform, same request shape, and Germany spent three
    // separate retunes (250 -> 500 -> 1000 -> 1400ms) discovering that anything
    // faster sits above what a Webedia origin tolerates sustained. Starting a
    // sibling market at the pace its sibling converged on costs Spain some sweep
    // length and risks nothing; starting it fast would rediscover the same
    // blackout-and-breaker cycle on a second country.
    //
    // It is only a STARTING point, and it is cheap to move: a Spanish venue
    // advertises far fewer days than a German one (measured 2026-09-01 over 30
    // venues: mean 7.8 requests per venue including its listing fetch, max 19,
    // against Germany's 13.4), so the sweep is 594 x 12 x 1400ms = ~2.8h inside a
    // 420min cadence — a ~40% duty cycle, well clear of the 100% the UK tried and
    // reverted. If Spain proves to tolerate more, KINOWO_SENSACINE_PACE_MS retunes
    // it live, and WorkerScrapeCadenceConfigSpec fails if the pace and the cadence
    // in movies-gitops/worker/overlays/es/patch.yaml ever drift apart.
    HostPolicy(
      Set("sensacine.com"),
      minRequestInterval = Some(Duration.ofMillis(1400)),
      paceKnob           = Some("KINOWO_SENSACINE_PACE_MS"),
    ),

    // Flicks (www.flicks.co.uk) — 500 UK venues, each fanning out one sessions
    // request per advertised day (~36 at the measured ~35 days/venue, so ~18k
    // requests per cycle) onto ONE origin. It was 843 venues until the chains went
    // own-site-primary on 2026-07-27; they now only reach flicks after 6h of a
    // failing primary. Like Filmstarts this was UNPACED, so the
    // 4-worker pool's fan-out delivered those in bursts Flicks answered with 429 —
    // panel-14 of kinowo-worker-diag showed a steady ~3-4% throttle spiking to
    // ~100% (pace-report: "8 requests, 8 throttled, pace=unpaced"). Worse than
    // Filmstarts', Flicks' limiter answers with Retry-After: 300-600s, so a burst
    // costs whole venue-days: ThrottledHttpFetch's 4 retries all fell inside that
    // window and the chunk was dropped (data loss + retry churn back onto the
    // queue). The 429s were purely self-inflicted (no 403s → not a Fly-ASN block),
    // so pacing the origin is the fix. 200ms (~5 req/s) is just under the ~6.4
    // req/s the unpaced pool averaged, but — the point — it SERIALISES the fan-out
    // so the concurrent bursts that trip the limiter never form. The pace is GLOBAL
    // per host (RateLimitedHttpFetch: one token bucket, not one per worker), so it
    // sets the sweep length: ~18k requests x 200ms = ~60min, and the UK overlay's
    // cadence must exceed that (the same pace↔cadence coupling DE has;
    // WorkerScrapeCadenceConfigSpec locks both). KINOWO_FLICKS_PACE_MS retunes it
    // live: if 200ms still throttles, step it down (and bump the cadence to match).
    //
    // The cadence is 420min, so that ~60min sweep leaves the pacer idle ~86% of the
    // time: ~62k requests/day at this origin. Keep it that way. An hourly cadence was
    // tried on 2026-07-28 and reverted the same day — it made the sweep exactly fill
    // its window, putting this pacer at a 100% duty cycle: a sustained ~5 req/s,
    // ~432k requests/day, against ONE third-party origin. The 200ms was tuned against
    // BURST-shaped 429s, and a flat 24/7 load is a different profile — a longer-window
    // quota (hourly/daily) would surface there rather than in the burst behaviour it
    // was fitted to. If the cadence is ever shortened again, watch panel-14 of
    // kinowo-worker-diag for the throttle % rather than assuming this pace still holds.
    HostPolicy(
      Set("flicks.co.uk"),
      minRequestInterval = Some(Duration.ofMillis(200)),
      paceKnob           = Some("KINOWO_FLICKS_PACE_MS"),
    ),
    // Flicks' US market (`flicks.us`) — the SAME platform on a different ccTLD,
    // and it needs its own row precisely BECAUSE it is a different host.
    //
    // The two markets are INDEPENDENT — at all three layers, and the third was
    // MEASURED rather than assumed (2026-08-30, before the US rollout):
    //   1. our pace gate (RateLimitedHttpFetch) buckets by full lowercased
    //      hostname, so the two hosts hold separate slot queues;
    //   2. our 429 back-off (ThrottledHttpFetch) keys the same way, so a
    //      `Retry-After: 300-600s` earned on one host cannot stall the other;
    //   3. THE ORIGIN appears to throttle per zone rather than per client IP.
    //      Driving flicks.us at 3-4.3 req/s across 30 concurrent workers (~1300
    //      requests) left flicks.co.uk polled alongside it completely flat — p50
    //      1.3s, p90 1.8s, max 2.0s, zero non-200s in 56 control polls,
    //      indistinguishable from its ~1.0s idle baseline — while the US host
    //      being hammered degraded to p50 3.7s / p90 13.2s / p99 39.6s in the
    //      same window. Note the LIMIT of that evidence: the US host only ever
    //      STALLED, it never returned a hard 429/403, so what is measured is
    //      "US load at production-like rates does not touch the UK", not "a
    //      fully blocked US leaves the UK clean". The asymmetry is strong
    //      evidence for per-zone limiting; it is not proof.
    // Layers 1 and 2 are decisive on their own, though, and they are the ones we
    // control: even if the origin turned out to share a quota, our own gates keep
    // the two sweeps from spending each other's budget or inheriting each other's
    // back-off.
    //
    // Note HOW this origin throttles: it STALLS connections rather than
    // answering 429. Its effective throughput plateaus at ~3-5 req/s no matter
    // the concurrency (10 workers → 2.88 req/s, 30 workers → 3.14 req/s), so
    // there is no faster pace to buy here — extra egress IPs would not raise it,
    // because the ceiling is per zone, not per IP.
    //
    // The flip side of host-keying is that policy rows match by host SUFFIX —
    // `flicks.co.uk` does not match `flicks.us` — so without this row the US
    // host would be entirely UNPACED, which is the exact condition that produced
    // the UK's self-inflicted 429 storm above, at six times the venue count.
    //
    // Same 200ms as the UK, and the measurement above says that IS the ceiling:
    // the origin plateaus at ~3-5 req/s and absorbs anything more by stalling,
    // so a shorter interval would buy latency, not throughput. It sets the sweep
    // length — ~5000 venues x ~36 requests x 200ms is a ~10h sweep, which is why
    // the US cadence is 840min rather than the UK's 420. See the US worker
    // overlay, and WorkerScrapeCadenceConfigSpec, which locks pace to cadence.
    //
    // What the two markets DO still share is the residential egress: both leave
    // over the Decodo pool, so the same IPs would carry both sweeps unless the US
    // worker is given its own. That coupling is addressed at the egress
    // (`WorkerWiring.flicksFetch` / `residential-proxy.properties`), not here —
    // this pacer cannot see which IP a request leaves on.
    HostPolicy(
      Set("flicks.us"),
      minRequestInterval = Some(Duration.ofMillis(200)),
      paceKnob           = Some("KINOWO_FLICKS_US_PACE_MS"),
    ),

    // ── US mid-tier chain origins ────────────────────────────────────────────
    // Each of the three rows below exists because rows match by host SUFFIX and
    // NOTHING here matches these hosts otherwise — `showcasecinemas.co.uk` does
    // not cover `showcasecinemas.com`, and no row at all means NO PACE. That is
    // the precise condition that produced the UK's self-inflicted 429 storm on
    // flicks.co.uk (see the long note above), so a new chain origin gets a row in
    // the same commit that starts fetching it, not after the first incident.
    //
    // These are all far LIGHTER than the aggregator they take work away from,
    // which is the whole point of moving the venues: 40 Alamo venues cost 40
    // requests a sweep (one per venue — the venue's entire programme comes back
    // in a single response) and the 39 Webedia venues cost 78 (catalogue +
    // schedule), against the ~36 per venue those same 79 venues were costing on
    // flicks.us. So the pace is a courtesy bound on a small load, not a throttle
    // fitted to an observed limit — none of the three has shown one. Each still
    // names its own knob so it can be tightened from /admin/config without a
    // redeploy if that ever changes.
    //
    // 500ms (2 req/s) is the rate the reconnaissance ran at, which every one of
    // these origins served without a single 403, 429 or stall. A 40-request
    // sweep at that pace is 20 seconds, so there is no cadence coupling to worry
    // about here the way there is for Flicks and Filmstarts — the sweep is
    // negligible against any cadence the US worker could plausibly run.
    HostPolicy(
      Set("drafthouse.com"),
      minRequestInterval = Some(Duration.ofMillis(500)),
      paceKnob           = Some("KINOWO_ALAMO_PACE_MS"),
    ),
    HostPolicy(
      Set("showcasecinemas.com"),
      minRequestInterval = Some(Duration.ofMillis(500)),
      paceKnob           = Some("KINOWO_SHOWCASE_US_PACE_MS"),
    ),
    HostPolicy(
      Set("landmarktheatres.com"),
      minRequestInterval = Some(Duration.ofMillis(500)),
      paceKnob           = Some("KINOWO_LANDMARK_PACE_MS"),
    ),
  )

  /** True when `url`'s host matches one of `suffixes` (exact host or a dotted
   *  sub-domain, so www.x matches x but an unrelated *.y does not). Swallows a
   *  malformed URL so routing never changes failure semantics — the subsequent
   *  buildRequest's own URI.create throws it the same way. */
  private def hostMatches(url: String, suffixes: Set[String]): Boolean =
    scala.util.Try(Option(URI.create(url).getHost)).toOption.flatten.exists { host =>
      val lowerHost = host.toLowerCase
      suffixes.exists(suffix => lowerHost == suffix || lowerHost.endsWith("." + suffix))
    }

  /** The first host policy matching `url`, if any. */
  private def policyFor(url: String): Option[HostPolicy] =
    all.find(policy => hostMatches(url, policy.hostSuffixes))

  /** The minimum gap between two outbound requests to `url`'s host, if that host
   *  is paced. `None` — the default for every host without a row naming one —
   *  means [[RateLimitedHttpFetch]] passes the call straight through.
   *
   *  Resolved PER REQUEST rather than baked into the table at class-init, so a
   *  `paceKnob` flip on `/admin/config` takes effect without a worker restart.
   *  The read is a map lookup against the override cache — cheap enough to sit
   *  on the request path, and it only runs for the few hosts that are paced. */
  def requestIntervalFor(url: String): Option[Duration] =
    policyFor(url).flatMap(tunedInterval)

  /** A policy's live pace: its knob's current value if it names one, else the
   *  compiled-in default. `Env.positiveLong` ignores a non-positive or
   *  unparseable override, so a fat-fingered `0` falls back to the default
   *  rather than silently unpacing a host we know we out-run. */
  private def tunedInterval(policy: HostPolicy): Option[Duration] =
    policy.paceKnob match {
      case None      => policy.minRequestInterval
      case Some(key) =>
        val compiledIn = policy.minRequestInterval.map(_.toMillis).getOrElse(0L)
        Some(Duration.ofMillis(Env.positiveLong(key, compiledIn)))
    }

  /** The connect (TCP+TLS handshake) budget for `url`: the matching host policy's,
   *  else the tight default. */
  def connectTimeoutFor(url: String): Duration =
    policyFor(url).map(_.connectTimeout).getOrElse(DefaultConnectTimeout)

  /** The per-request (response-read) budget for `url`: the matching host policy's,
   *  else the generous default. */
  def requestTimeoutFor(url: String): Duration =
    policyFor(url).map(_.requestTimeout).getOrElse(DefaultRequestTimeout)

  /** The headers `url`'s host demands on every request, if it names any. Empty —
   *  the default for every host without a row naming one — leaves the request
   *  exactly as the defaults built it. */
  def headersFor(url: String): Map[String, String] =
    policyFor(url).map(_.headers).getOrElse(Map.empty)
}
