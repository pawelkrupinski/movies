package services.cinemas

import models._
import services.cinemas.common.{CinemaScraper, FlicksClient, GatsbyBoxOfficeClient, VueCinemasPlatformClient}
import services.cinemas.uk.{BfiSouthbankClient, CineworldClient, NovaCinemaClient, OdeonClient, QuadDerbyClient, SavoyClient, SpektrixClient, TheOldCourtClient, TicketsolveClient, UkFeed, UkSourcesData, UkVenueSources}

import java.time.LocalDate

/**
 * Every UK venue's scraper, as [[CinemaScraperCatalog]] files them: the chains' own sites and the
 * independents' box offices as PRIMARY, flicks.co.uk (`flicks`, built by the catalog: its film pages
 * are shared with the US market) for the rest. Which source each venue has is data
 * (`data/uk/roster.json`, generated into [[UkSourcesData]]); the page a venue is listed on is
 * `models.UkPages`.
 */
final class UkScraperCatalog(egress: CinemaEgress, venueClock: VenueClock, flicks: (String, Cinema) => FlicksClient) {
  private val http           = egress.direct
  private val flicksFetch    = egress.flicks
  private val vueFetch       = egress.vue
  private val odeonFetch     = egress.odeon
  private val odeonAuthToken = egress.odeonAuthToken

  /** Today in the UK, asked afresh at every read. */
  private def today: LocalDate = venueClock.today(TimeZones.UnitedKingdom)

  /** Every UK venue's scraper, in roster.json's order — its groups' order is each page's scrape
   *  order, so moving a group reorders the scrapers of every page it shares. */
  val scrapers: Seq[CinemaScraper] = UkSourcesData.venues.map(scraper)

  private def scraper(venue: UkVenueSources): CinemaScraper = {
    val cinema = venue.cinema
    venue.primary match {
      case UkFeed.Flicks(slug) => flicks(slug, cinema)
      // UK chain own-site clients — the catalogue PRIMARY for their venues, with
      // flicks.co.uk kept as the aggregator fallback (see [[ChainFlicksFallback]] +
      // `WorkerWiring.recordingScraper`). Cineworld + Vue are Cloudflare-403'd from our
      // Fly datacenter IP (verified in prod 2026-07-27), so they egress residential:
      // Cineworld via `flicksFetch` (GET-only, per-venue sticky is fine), Vue via
      // `vueFetch` (host-sticky, for its token cookie), Odeon via `odeonFetch`.
      // Showcase/Everyman still reach the origin directly, so they use `http`. Any
      // proxy failure rolls to the flicks fallback.
      case UkFeed.Cineworld(slug) => new CineworldClient(flicksFetch, slug, cinema, today = today)
      case UkFeed.Vue(id) => new VueCinemasPlatformClient(vueFetch, VueCinemasPlatformClient.MyVueBaseUrl, id, cinema)
      case UkFeed.Showcase(id) =>
        new GatsbyBoxOfficeClient(http, GatsbyBoxOfficeClient.ShowcaseBaseUrl, id, cinema, today = today)
      case UkFeed.Everyman(id) =>
        new GatsbyBoxOfficeClient(http, GatsbyBoxOfficeClient.EverymanBaseUrl, id, cinema, today = today)
      // Odeon pulls Vista `ocapi` over the injected JWT (harvested via Zyte); only the
      // token harvest needs a browser, the data fetch needs only the bearer. The ocapi
      // host DOES sit behind Cloudflare though — it answered our Fly IP but 403s the
      // Hetzner egress the worker moved to on 2026-08-29 — so the data fetch goes over
      // the residential `odeonFetch`, not direct `http`. A missing token, or a proxy
      // that can't get through either, throws → flicks fallback.
      case UkFeed.Odeon(id) => new OdeonClient(odeonFetch, id, cinema, odeonAuthToken, today = today)
      // UK independents whose own box office (Savoy Systems, Spektrix, Ticketsolve — and
      // the single-venue clients below) lists what Flicks does not — the same own-site
      // PRIMARY + flicks FALLBACK arrangement as the chains above (see
      // [[ChainFlicksFallback.ukOwnSiteSlugs]]). Every one of these origins answers our
      // egress directly.
      case UkFeed.Savoy(dllBase) => new SavoyClient(http, dllBase, cinema, today = today)
      case UkFeed.SavoyBoxOffice(dllBase) => new SavoyClient(http, dllBase, cinema, today = today, SavoyClient.BoxOffice)
      case UkFeed.Spektrix(client, building) => new SpektrixClient(http, client, cinema, today = today, building)
      case UkFeed.Ticketsolve(client) => new TicketsolveClient(http, client, cinema, today = today)
      case UkFeed.QuadDerby => new QuadDerbyClient(http, today = today) // built for Derby QUAD, which it names itself
      case UkFeed.BfiSouthbank => new BfiSouthbankClient(http, cinema, today)
      case UkFeed.NovaCinema => new NovaCinemaClient(http, cinema, today)
      case UkFeed.TheOldCourt => new TheOldCourtClient(http, cinema, today)
    }
  }
}
