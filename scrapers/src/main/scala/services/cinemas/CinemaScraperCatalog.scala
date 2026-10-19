package services.cinemas

import models._
import tools.{CachingDetailFetch, HttpFetch}
import services.cinemas.common.{CinemaScraper, GatsbyBoxOfficeClient, WebediaMarket, WebediaShowtimesClient}
import services.cinemas.common.{CineamoClient, FlicksClient, FlicksMarket, KinoheldClient, KinoprogrammClient, WixEventsClient}
import services.cinemas.de.{ArsenalBerlinClient, GermanFeed, GermanSources, RexSchifferstadtClient}
import services.cinemas.es.{ArtesieteClient, OcineClient, SpanishOwnSourcesData, TaquillaClient, TaquillaVenue}
import services.cinemas.us.{AlamoDrafthouseClient, CinemaPlusClient, SquarespaceEventsClient, UsChainVenues, UsIndependentVenues}
import services.movies.TitleNormalizer

import java.time.Clock
import scala.concurrent.duration._

/**
 * The single place that constructs every cinema's live scraper and files them under
 * the page their cinema is on. Extracted out of `modules.WorkerWiring` so the scraper
 * graph can be built without the worker's Mongo / event-bus / enrichment machinery — a
 * diagnostic such as `tools.FilmwebDiff` needs only `(http) => Seq[CinemaScraper]`,
 * not the whole write composition root.
 *
 * Takes the seams the worker (and its fixture-replay test wiring) vary: the routes each
 * kind of venue fetches through ([[CinemaEgress]]); `venueClock` — which day each venue
 * is on, asked per scrape in the venue's own zone ([[VenueClock]]), Poland's being the
 * date Helios bakes into its REST URLs; the per-chain detail cache; and the title rules.
 * Poland's and the UK's hand-wired venues are [[PolishScraperCatalog]] and
 * [[UkScraperCatalog]]; Germany, Spain and the US are built off their rosters here.
 *
 * Returns RAW scrapers. `WorkerWiring` wraps each in a `RetryingCinemaScraper`
 * (retry) + `UptimeRecordingScraper` (record the outcome) for production scrape
 * ticks; a diagnostic uses them bare.
 */
class CinemaScraperCatalog(
  egress:     CinemaEgress,
  venueClock: VenueClock,
  // Builds the per-chain detail-page cache, taking the chain it is for. The worker
  // injects a Mongo-backed cache so chain detail is deduped across servers; the
  // diagnostic ctor + tests use the in-process CachingDetailFetch. The chain name
  // is not decoration — each chain's cache carries its own TTL, so each needs its own
  // store (see `MongoCachingDetailFetch.collectionName`).
  chainDetailCache: (String, HttpFetch, FiniteDuration) => HttpFetch,
  // The title rules the scrapers below clean their raw titles with. Country-scoped:
  // the " & " → " i " unification is Poland-only, and applying it to a German venue
  // re-keys "Minions & Monster" as `minionsimonster`. `WorkerWiring` passes the rule
  // set for the country it was booted for; the diagnostic ctor defaults to Poland's.
  titles: TitleNormalizer
) {

  private val http        = egress.direct
  private val flicksFetch = egress.flicks

  /** Diagnostic ctor (`FilmwebDiff`, `RosterAudit`, the slice and replay tools, specs): the routes
   *  [[CinemaEgress.diagnostic]] builds, and an in-process detail cache. Kept in this shape because
   *  the identity tools compile their files into older refs, where it is the catalog's only door. */
  def this(http: HttpFetch, venueClock: VenueClock, clock: Clock,
           titles: TitleNormalizer = TitleNormalizer.forCountry(Country.default),
           proxyShards: Option[IndexedSeq[HttpFetch]] = None) =
    this(CinemaEgress.diagnostic(http, clock, proxyShards), venueClock, (_, h, ttl) => new CachingDetailFetch(h, ttl), titles)

  /** Poland's hand-wired venues. */
  val poland: PolishScraperCatalog = new PolishScraperCatalog(egress, venueClock, chainDetailCache, titles)

  // ── United Kingdom (Flicks) ──────────────────────────────────────────────
  // Through `flicksFetch`, NOT `http`: Cloudflare 403s our Fly egress IP on
  // flicks.co.uk, and Flicks is the only UK source, so a direct fetch takes out
  // every UK venue at once (it did, 2026-07-26).
  private def flicks(slug: String, cinema: Cinema): FlicksClient =
    flicksIn(FlicksMarket.UnitedKingdom, slug, cinema)

  // The US runs on the same Flicks platform as the UK, so it reuses the same
  // client and the same residential egress — only the market (and so the host)
  // differs. It is a SEPARATE hostname, which is what keeps the two markets'
  // pace gates and 429 back-offs independent; see `FlicksMarket`.
  private def flicksUs(slug: String, cinema: Cinema): FlicksClient =
    flicksIn(FlicksMarket.UnitedStates, slug, cinema)

  private def flicksIn(market: FlicksMarket, slug: String, cinema: Cinema): FlicksClient =
    FlicksClient.forVenue(flicksFetch, flicksFilmPages, slug, cinema, market, venueClock)

  /** Where every Flicks client — each market's venues, and a chain venue's Flicks fallback — reads its films'
   *  `/movie/<slug>/` pages: one page per FILM per market, the same at every venue listing it, so one cache keyed
   *  by the page URL (market host + slug) serves them all, across worker servers. Through `flicksFetch`, the
   *  residential egress the listings need too. */
  val flicksFilmPages: HttpFetch = chainDetailCache("flicks", flicksFetch, FlicksClient.DetailTtl)

  /** The UK's hand-wired venues. */
  val uk: UkScraperCatalog = new UkScraperCatalog(egress, venueClock, flicks)

  // ── Germany (AlloCiné/Filmstarts website-JSON) ───────────────────────────
  private def filmstarts(theaterId: String, cinema: Cinema): WebediaShowtimesClient =
    WebediaShowtimesClient.forVenue(http, WebediaMarket.Germany, theaterId, cinema, venueClock)
  /** A German venue's scraper for one of its sources ([[GermanFeed]]) — its primary in the
   *  catalogue, or a link of its fallback chain (`ScrapeWiring`). */
  def germanFeedScraper(feed: GermanFeed, cinema: Cinema): CinemaScraper = feed match {
    case GermanFeed.Filmstarts(theaterId) => filmstarts(theaterId, cinema)
    case GermanFeed.Cineamo(id)           => new CineamoClient(http, id, cinema, today = venueClock.todayAt(cinema, CineamoClient.Zone))
    case GermanFeed.Kinoheld(venue)       => new KinoheldClient(http, venue, cinema, today = venueClock.todayAt(cinema, KinoheldClient.Zone))
    case GermanFeed.Kinoprogramm(path)    =>
      new KinoprogrammClient(http, path, cinema, today = venueClock.today(KinoprogrammClient.Zone), filmPages = kinoprogrammFilmPages)
    case GermanFeed.WixEvents(page, postcode) =>
      new WixEventsClient(http, page, postcode, TimeZones.Germany, cinema, today = venueClock.todayAt(cinema, TimeZones.Germany))
    case GermanFeed.RexSchifferstadt(page) =>
      new RexSchifferstadtClient(http, page, cinema, today = venueClock.todayAt(cinema, TimeZones.Germany))
    case GermanFeed.ArsenalBerlin =>
      new ArsenalBerlinClient(http, cinema, today = venueClock.todayAt(cinema, TimeZones.Germany))
  }

  /** Each rostered German venue's sources, primary first ([[GermanSources]]: the per-venue
   *  choice `data/germany/scripts/harvest_sources.py` measured). */
  private def germanFeeds(cinema: Cinema): Seq[GermanFeed] =
    GermanSources.of(models.GermanRoster.venueKeyByCinema(cinema), models.GermanRoster.kinoprogrammPathByCinema.get(cinema))

  // Germany — data-driven from the full GermanRoster (436 town pages / 1,515 cinemas):
  // one scraper per cinema — its primary source's ([[germanFeeds]]) — keyed by its page.
  private val germanBaseByCity: Map[City, Seq[CinemaScraper]] =
    models.Country.Germany.cities.map { city =>
      city -> city.cinemas.map(c => germanFeedScraper(germanFeeds(c).head, c))
    }.toMap

  // ── Spain (AlloCiné/SensaCine website-JSON) ──────────────────────────────
  // The SAME client Germany uses, on a different market — so a different HOST,
  // which is what keeps the two countries' pace gates and 429 back-offs
  // independent of each other; see `WebediaMarket`.
  def sensacineScraper(theaterId: String, cinema: Cinema): WebediaShowtimesClient = sensacine(theaterId, cinema)
  private def sensacine(theaterId: String, cinema: Cinema): WebediaShowtimesClient =
    WebediaShowtimesClient.forVenue(http, WebediaMarket.Spain, theaterId, cinema, venueClock)

  // Ocine venues read the chain's own ticketing server instead — SensaCine had
  // no programme at all for most of them, and does not list some at all; see
  // `data/spain/ocine.json` for which and why. Plain `http`: the servers sit
  // behind no bot wall.
  private def ocine(ticketingSlug: String, cinema: Cinema): OcineClient =
    new OcineClient(http, ticketingSlug, cinema, today = venueClock.todayAt(cinema, OcineClient.Zone))

  // Chain multiplexes SensaCine lists no programme for read taquilla.com instead,
  // the aggregator that sells their tickets — measured per venue by
  // `data/spain/scripts/harvest_taquilla.py`; see `data/spain/taquilla.json`.
  private def taquilla(venue: TaquillaVenue, cinema: Cinema): TaquillaClient =
    new TaquillaClient(http, venue, cinema, today = venueClock.todayAt(cinema, TaquillaClient.Zone))

  // Artesiete venues read the chain's own venue page — two months of sessions with
  // room and format, against SensaCine's weeks; see `data/spain/artesiete.json`.
  private def artesiete(page: String, cinema: Cinema): ArtesieteClient =
    new ArtesieteClient(http, page, cinema)

  /** A SensaCine-listed Spanish venue's scraper: its chain's own page or taquilla.com
   *  where [[SpanishOwnSourcesData]] names one, SensaCine otherwise. */
  private def spanishListed(theaterId: String, cinema: Cinema): CinemaScraper =
    SpanishOwnSourcesData.artesiete.get(theaterId).map(artesiete(_, cinema))
      .orElse(SpanishOwnSourcesData.taquilla.get(theaterId).map(taquilla(_, cinema)))
      .getOrElse(sensacine(theaterId, cinema))

  // Spain — data-driven from the full SpanishRoster (240 town pages / 599 cinemas):
  // one scraper per cinema, keyed by its page — the venue's own chain server where the
  // roster names one, then its chain's own page or taquilla.com where
  // `SpanishOwnSourcesData` names one, SensaCine otherwise.
  private val spanishBaseByCity: Map[City, Seq[CinemaScraper]] =
    models.Country.Spain.cities.map { city =>
      city -> city.cinemas.map { c =>
        models.SpanishRoster.ocineServerByCinema.get(c).map(ocine(_, c))
          .getOrElse(spanishListed(models.SpanishRoster.theaterIdByCinema(c), c))
      }
    }.toMap

  // ── United States (chain-primary, Flicks for the rest) ───────────────────
  // Data-driven from the full UsRoster (1,995 town and cluster pages / 4,999 cinemas):
  // one scraper per cinema, keyed by the PAGE slug City.slug uses.
  //
  // A venue named in `UsChainVenues` gets its CHAIN'S OWN site as the primary and
  // keeps flicks.us as the fallback (below); one named in `UsIndependentVenues` gets
  // its own site with no fallback (Flicks dropped it); every other venue is flicks.us
  // primary as before, its slug from UsRoster.flicksSlugByCinema. Same client and
  // same residential egress as the UK for the Flicks leg — only the market
  // differs, which is what keeps the two markets' pace gates and 429 back-offs
  // independent (see FlicksMarket).
  //
  // WHY THESE CHAINS AND NOT THE OTHERS. Moving a venue off the aggregator is only
  // safe if the chain's own feed advertises AT LEAST as much of the programme:
  // `MovieCache`'s scrape-prune reads a film's absence from a complete listing as
  // "it stopped screening", so a shorter-horizon primary DELETES the advance-sale
  // tail on every successful scrape (see ScrapeHorizon — it cost the UK its whole
  // event programme once already). All three chains here were measured against
  // flicks.us on the same venues on 2026-08-30 and each reached the same furthest
  // date or better, with equal or more populated days. Regal and AMC were measured
  // too, but their own sites answer only US IPs, so both stay on flicks.us
  // (see `RegalClient` / `AmcClient`, unwired). The other four US mid-tier chains are NOT here, and
  // neither is Cinemark (38-193 days SHORTER than flicks on all 20 venues
  // measured) — `UsChainVenues` names each one and why.
  //
  // Alamo reaches our datacenter egress directly (no Cloudflare challenge from it
  // on 2026-08-30), as do both Webedia hosts — so all three use `http`, not the
  // residential `flicksFetch` the Flicks leg needs.
  /** Alamo's per-show detail, shared by all 40 venues: one show body per TTL, whichever venue asks first.
   *  Public because the identity's catalogue-link reader reads the same bodies. */
  val alamoDetailHttp: HttpFetch = chainDetailCache("alamo-drafthouse", http, AlamoDrafthouseClient.DetailTtl)
  private def alamo(venue: UsChainVenues.AlamoVenue, cinema: Cinema): AlamoDrafthouseClient =
    new AlamoDrafthouseClient(http, alamoDetailHttp, venue.slug, cinema, today = venueClock.today(TimeZones.named(venue.zoneId)))
  private def webedia(baseUrl: String, venue: UsChainVenues.WebediaVenue, cinema: Cinema): GatsbyBoxOfficeClient =
    new GatsbyBoxOfficeClient(http, baseUrl, venue.theaterId, cinema,
      timeZone = venue.zoneId, venuePath = Some(venue.venuePath), today = venueClock.today(TimeZones.named(venue.zoneId)),
      ageRatings = services.cinemas.common.GatsbyBoxOfficeParser.MpaCertificates)

  /** The chain-primary scraper for a US venue, or `None` when it stays on Flicks.
   *
   *  The chains key off the venue's DISPLAY NAME (their own rosters name venues,
   *  not slugs), an exact lookup — a venue absent from every map falls through to
   *  Flicks, which is the correct answer for the chain locations whose operator no
   *  longer lists them. */
  private def usChainScraper(cinema: Cinema): Option[CinemaScraper] = {
    val name = cinema.displayName
    UsChainVenues.alamoDrafthouse.get(name).map(alamo(_, cinema))
      .orElse(UsChainVenues.showcaseUs.get(name).map(webedia(GatsbyBoxOfficeClient.ShowcaseUsBaseUrl, _, cinema)))
      .orElse(UsChainVenues.landmark.get(name).map(webedia(GatsbyBoxOfficeClient.LandmarkBaseUrl, _, cinema)))
  }

  /** The own-site primary for a US independent Flicks no longer lists, or `None`.
   *  No Flicks fallback: see `UsIndependentVenues`. Its own site answers our
   *  datacenter egress directly (200 from the fleet, 2026-10-12), so plain `http`. */
  private def usIndependentScraper(cinema: Cinema): Option[CinemaScraper] = {
    val zone = VenueClock.zoneOf(cinema, TimeZones.UsEastern)
    UsIndependentVenues.squarespace.get(cinema.displayName).map[CinemaScraper] { venue =>
      new SquarespaceEventsClient(http, venue.siteUrl, venue.eventsPath, cinema, zone, today = venueClock.today(zone))
    }.orElse(UsIndependentVenues.cinemaPlus.get(cinema.displayName).map { venue =>
      new CinemaPlusClient(http, venue.siteUrl, venue.venuePath, cinema, today = venueClock.today(zone))
    })
  }

  private val usBaseByCity: Map[City, Seq[CinemaScraper]] =
    models.Country.UnitedStates.cities.map { city =>
      city -> city.cinemas.map { c =>
        usChainScraper(c).orElse(usIndependentScraper(c)).getOrElse(flicksUs(models.UsRoster.flicksSlugByCinema(c), c))
      }
    }.toMap

  /** US chain venues → the flicks.us slug they keep as their FALLBACK.
   *
   *  Derived from the same `UsRoster.flicksSlugByCinema` their Flicks primary used
   *  to be built from, rather than restated in a table: a US venue is a runtime
   *  object with no case-object name to write down, and deriving means the primary
   *  and the fallback cannot drift apart about which venue they mean. */
  private val usFlicksFallback: Map[Cinema, ChainFlicksFallback.FlicksFallback] =
    models.Country.UnitedStates.cities.flatMap(_.cinemas)
      .filter(c => usChainScraper(c).isDefined)
      .flatMap(c => models.UsRoster.flicksSlugByCinema.get(c)
        .map(slug => c -> ChainFlicksFallback.FlicksFallback(FlicksMarket.UnitedStates, slug)))
      .toMap

  /** Every city's scrapers, keyed by the CITY — never its slug, which only means
   *  something within a country (`york` is a UK page and a US one).
   *
   *  The hand-wired ones (Poland's and the UK's, with Poland's Filmweb sweep and
   *  nearby towns) are filed under the PAGE their cinema is on, not the group they
   *  are wired in above: which page lists which venue is data (`models.PolishPages`
   *  and `models.UkPages`, re-clustered by each country's `build_pages.py`), and a
   *  re-cluster must not need a single scraper moved here to take effect. The
   *  data-driven countries (Germany's 448, the US's 1,995, Spain's 240 pages) are
   *  built per city to begin with. */
  private val wiredGroups: Seq[Seq[CinemaScraper]] =
    Seq(poland.scrapers, uk.scrapers) ++ (germanBaseByCity.values ++ usBaseByCity.values ++ spanishBaseByCity.values).toSeq
  private val filed = CinemaScraperCatalog.file(wiredGroups, City.forCinema)

  val byCity: Map[City, Seq[CinemaScraper]] = filed.byCity

  /** Every scraper built here whose cinema no page lists — scraped by nothing, so
   *  `CinemaScraperCoverageSpec` holds it empty rather than letting one vanish. */
  val unplaced: Seq[CinemaScraper] = filed.unplaced

  /** How many scrapers the catalog built, every group counted. */
  private[cinemas] val built: Int = wiredGroups.map(_.size).sum

  /** Every raw scraper across every city, in city order. */
  val all: Seq[CinemaScraper] =
    City.all.flatMap(c => byCity.getOrElse(c, Nil))

  /** UK chain venues (Cineworld / Vue / Showcase / Everyman / Odeon) whose own-site
   *  client is the catalogue's primary and that keep flicks.co.uk as their aggregator
   *  FALLBACK — the mirror of the Polish own-site→Filmweb arrangement. Maps each such
   *  cinema to the flicks slug it used to be catalogued under, so `WorkerWiring` can
   *  build the fallback `FlicksClient` on demand. Populated by the chain-wiring step;
   *  empty until then (behaviour identical to the pre-chain flicks-primary catalogue). */
  val flicksFallbackSlugs: Map[Cinema, ChainFlicksFallback.FlicksFallback] =
    ChainFlicksFallback.slugs ++ usFlicksFallback

  /** Each German venue's FALLBACK chain — its sources after the primary, in order
   *  ([[GermanSources]]); absent for a venue with only the one source. */
  val germanFallbackFeeds: Map[Cinema, Seq[GermanFeed]] =
    models.Country.Germany.cities.flatMap(_.cinemas).map(c => c -> germanFeeds(c).tail).filter(_._2.nonEmpty).toMap

  /** Each SensaCine-listed Spanish venue read off another source — taquilla.com or
   *  Artesiete's own page ([[SpanishOwnSourcesData]]) — by the SensaCine theaterId its
   *  FALLBACK reads ([[sensacineScraper]]): where it was scraped before, and what the
   *  other source had to beat to take it over (`data/spain/taquilla.json`,
   *  `data/spain/artesiete.json`). */
  val spanishSensacineFallback: Map[Cinema, String] =
    models.Country.Spain.cities.flatMap(_.cinemas)
      .flatMap(c => models.SpanishRoster.theaterIdByCinema.get(c)
        .filter(SpanishOwnSourcesData.ownSourced).map(c -> _))
      .toMap

  /** Where every kinoprogramm.com fallback reads its films' catalogue pages: one page per
   *  FILM, the same for every venue showing it, so one cache serves them all. Without it a
   *  whole-Filmstarts outage would fetch every listed film's page at each of 1,500 venues,
   *  where the cache asks each film once per sweep. The TTL stays under the 6h
   *  `DetailEnrich` window every chain cache keeps to, and outlasts one outage sweep. */
  lazy val kinoprogrammFilmTtl: FiniteDuration = 5.hours
  // Lazy, both: the German venues built above may already read them (a kinoprogramm.com primary).
  lazy val kinoprogrammFilmPages: HttpFetch = chainDetailCache("kinoprogramm", http, kinoprogrammFilmTtl)

  /** Union of every cinema scraper's HTTP hosts. `MonitoringHttpFetch`
   *  suppresses per-host uptime rows for these — each cinema's health is
   *  already tracked under its `displayName` by `UptimeRecordingScraper`, so a
   *  per-host row would be a duplicate landing in the uptime page's "Other"
   *  bucket. Single source of truth: a new cinema's client declares its host
   *  (forced by the abstract `CinemaScraper.scrapeHosts`) and is suppressed
   *  automatically — no hand-kept host list to drift. */
  val scrapeHosts: Set[String] = all.flatMap(_.scrapeHosts).toSet ++
    // A fallback feed is scraped for a venue too, but by no catalogue scraper.
    germanFallbackFeeds.toSeq.flatMap { case (c, feeds) => feeds.flatMap(germanFeedScraper(_, c).scrapeHosts) }.toSet

  /** The hosts of the ticketing platforms' venue pages read through an egress other than direct, each by that
   *  egress ([[PolishScraperCatalog.egressByHost]]). */
  val egressByHost: Map[String, HttpFetch] = poland.egressByHost
}

object CinemaScraperCatalog {

  /** [[file]]'s answer: every scraper under the page its cinema is on, and the ones
   *  whose cinema is on no page. */
  final case class Filed(byCity: Map[City, Seq[CinemaScraper]], unplaced: Seq[CinemaScraper])

  /** File `groups` of scrapers under the PAGE each one's cinema is on (`pageOf`) —
   *  whichever group it was wired in — keeping every one: scrapers of one page from
   *  several groups sit side by side, in wiring order, and a scraper whose cinema is
   *  on no page is kept as [[Filed.unplaced]] rather than dropped. */
  def file(groups: Seq[Seq[CinemaScraper]], pageOf: Cinema => Option[City]): Filed = {
    val all = groups.flatten
    val (placed, unplaced) = all.partition(s => pageOf(s.cinema).isDefined)
    Filed(placed.groupBy(s => pageOf(s.cinema).get), unplaced)
  }

  /** Kino Kryterium's MSI portal: it times out the worker's IP and every Decodo proxy IP, so its pages AND its posters
   *  go through the Zyte route (`EgressWiring.zyteFetch`). */
  val KinoKryteriumUrl = "https://bilety.ck105.koszalin.pl"
}
