package services.cinemas.common

import java.time.ZoneId

/**
 * One national deployment of the Flicks (Vista) listings platform.
 *
 * Flicks runs the SAME site — same markup, same `/cinema/<slug>/` day tabs, same
 * `/cinema/sessions/<slug>/<date>/` AJAX fragment — on four separate ccTLDs
 * (`flicks.co.uk`, `flicks.us`, `flicks.co.nz`, `flicks.com.au`). Only the host
 * and the local calendar differ, so [[FlicksClient]] is parameterised by this
 * rather than duplicated per country.
 *
 * Each market is a SEPARATE Cloudflare zone on its own hostname, which is what
 * makes them independent to everything downstream that keys by host: the
 * per-host pace gate ([[tools.RateLimitedHttpFetch]]) and the 429 back-off
 * ([[tools.ThrottledHttpFetch]]) both bucket by full lowercased hostname, so a
 * `Retry-After` earned on `www.flicks.co.uk` never stalls `www.flicks.us` and
 * neither market spends the other's request budget. The corollary is that each
 * market needs its OWN `HostPolicy` row in [[tools.RealHttpFetch]] — a host with
 * no row is not paced at all, and the UK's row does not match the US host.
 *
 * `zoneId` is the market's reference time zone, used only to resolve "today"
 * when a client is built without an explicit date. It is a per-market DEFAULT,
 * not a claim that the country has one zone: the US spans six, so a US venue's
 * scraper is handed its city's own zone by the catalog. Getting it wrong costs
 * at most a day-boundary tab, never a wrong showtime — Flicks prints each
 * session in the venue's own local time and we store it as a `LocalDateTime`.
 */
sealed abstract class FlicksMarket(
  val baseUrl: String,
  val zoneId:  ZoneId,
) {
  /** The hostname this market is served from — the key the pace gate and the
   *  429 back-off bucket by, and the value a `HostPolicy` row must name. */
  def host: String = baseUrl.stripPrefix("https://").stripPrefix("http://")
}

object FlicksMarket {

  case object UnitedKingdom extends FlicksMarket(
    baseUrl = "https://www.flicks.co.uk",
    zoneId  = ZoneId.of("Europe/London"),
  )

  /** The US market. The same site as the UK's, but SIX TIMES the corpus —
   *  ~5000 venues in `sitemap-cinemas.xml` against the UK's ~850 — so it is a
   *  scrape-VOLUME problem before it is a client problem. Its cadence and pace
   *  are budgeted separately for that reason; see `WorkerScrapeCadenceConfigSpec`.
   *  `America/New_York` is only the "today" default (see [[FlicksMarket.zoneId]]);
   *  each US city carries its own zone. */
  case object UnitedStates extends FlicksMarket(
    baseUrl = "https://www.flicks.us",
    zoneId  = ZoneId.of("America/New_York"),
  )

  val all: Seq[FlicksMarket] = Seq(UnitedKingdom, UnitedStates)
}
