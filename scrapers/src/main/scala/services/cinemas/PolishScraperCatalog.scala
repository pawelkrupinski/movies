package services.cinemas

import models.VenueClock
import tools.HttpFetch
import services.cinemas.common.{CinemaScraper, MultiListingScraper}
import services.cinemas.pl._
import services.cinemas.pl.PolishVenue.Route
import services.titles.TitleNormalizer

import java.time.LocalDate
import scala.concurrent.duration._

/**
 * Every Polish venue's scraper, as [[CinemaScraperCatalog]] files them: built from `venues`, the towns' venue
 * registries' data in scrape order ([[PolishVenueRegistry]]; the composition root passes `pl.venues.PolishTowns.venues`),
 * each venue bound here to the routes, the clock, the chains' detail caches and the title rules. A new venue is a line
 * in its town's registry; this class changes only for a new platform.
 */
final class PolishScraperCatalog(
  egress:           CinemaEgress,
  venueClock:       VenueClock,
  chainDetailCache: (String, HttpFetch, FiniteDuration) => HttpFetch,
  titles:           TitleNormalizer,
  venues:           Seq[PolishVenue],
  // Where a feed shared by many venues is read, off the scrape that asked (BiletynaNationalFeed).
  backgroundLoads:  java.util.concurrent.Executor
) {
  private val http      = egress.direct
  private val mkFetch   = egress.multikino
  private val bnFetch   = egress.biletyna
  private val zyteFetch = egress.zyte

  /** Today in Poland, asked afresh at every read: each client takes it by name, so a
   *  scraper built at boot still plans from the day it scrapes on. */
  private def today: LocalDate = venueClock.todayInPoland

  // Per-film detail bodies are static between passes and IDENTICAL across a
  // chain's locations, so each chain shares ONE CachingDetailFetch: a film's
  // detail (Helios `/api/movie/{id}`, Cinema City film page) is fetched once per
  // chain per TTL instead of once per location per pass. Live listing/screening
  // fetches stay on `http`.
  //
  // BOTH EXPIRE INSIDE THE `DetailEnrich` REFRESH WINDOW, and `CinemaScraperCatalogSpec`
  // holds them to it. Cinema City sat at 6h, EQUAL to that window, so whether a
  // scheduled refresh did any work was decided by which of the two elapsed first
  // — and on the losing side it re-parsed the bytes it already had, produced the
  // identical detail, and stamped it fresh. Helios earns its cache on a different
  // axis and keeps a shorter one still: it fetches movie and screen bodies from
  // `fetchRestData()`, INSIDE the scrape pass, once per id in the listing.
  val heliosDetailTtl:     FiniteDuration = 2.hours
  val cinemaCityDetailTtl: FiniteDuration = 2.hours
  private val heliosDetailHttp:     HttpFetch = chainDetailCache("helios", http, heliosDetailTtl)
  private val cinemaCityDetailHttp: HttpFetch = chainDetailCache("cinema-city", http, cinemaCityDetailTtl)
  private val cinemaCityClient: CinemaCityClient = new CinemaCityClient(http, Some(cinemaCityDetailHttp), titles = titles)

  private def biletynaPages(venue: PolishVenue): Seq[BiletynaPlacePage] = venue match {
    case PolishVenue.Biletyna(_, page)       => Seq(page)
    case PolishVenue.BiletynaHalls(_, halls) => halls.map(_._1)
    case _                                   => Nil
  }

  // One read of biletyna's national event feed serves every biletyna venue: ~7
  // proxied requests a pass instead of one (or more) per venue.
  private lazy val biletynaNationalFeed: BiletynaNationalFeed =
    new BiletynaNationalFeed(bnFetch, venues.flatMap(biletynaPages).toSet, backgroundLoads)

  private val kit = new PolishScraperKit(http, bnFetch, () => today, titles)

  private def fetchOf(route: Route): HttpFetch = route match {
    case Route.Direct => http
    case Route.Zyte   => zyteFetch
  }

  private def scraper(venue: PolishVenue): CinemaScraper = venue match {
    case chain: PolishVenue.Chain       => chainScraper(chain)
    case platform: PolishVenue.Platform => platformScraper(platform)
    case PolishVenue.OwnClient(build)   => build(kit)
  }

  /** A chain venue's scraper, over the chain's shared detail cache. */
  private def chainScraper(venue: PolishVenue.Chain): CinemaScraper = venue match {
    case PolishVenue.CinemaCity(cinemaId, cinema) => new CinemaCityScraper(cinemaCityClient, cinemaId, cinema)
    case PolishVenue.Multikino(cinemaId, cinema)  => new MultikinoClient(mkFetch, cinemaId, cinema, titles)
    case PolishVenue.Helios(config)               => new HeliosClient(http, config, today, Some(heliosDetailHttp), titles = titles)
  }

  /** A platform venue's scraper, over the egress its platform's host needs. */
  private def platformScraper(venue: PolishVenue.Platform): CinemaScraper = venue match {
    case PolishVenue.Bilety24Organizer(organizerUrl, cinema) =>
      new Bilety24OrganizerClient(http, organizerUrl, cinema, titles = titles)
    case PolishVenue.Bilety24Subdomain(cinema, url) => new Bilety24Client(http, url, cinema, titles = titles)
    case PolishVenue.Ekobilet(slug, cinema)         => new EkobiletClient(http, slug, cinema, today)
    case PolishVenue.NoveKino(slug, cinema)         => new NoveKinoClient(http, slug, cinema)
    case PolishVenue.Filmweb(filmwebCinemaId, cinema) => new FilmwebShowtimesClient(http, filmwebCinemaId, cinema)
    case msi: PolishVenue.Msi =>
      new MsiClient(fetchOf(msi.route), msi.baseUrl, msi.cinema, today,
        mvcPath = msi.mvcPath, titlePrefix = msi.titlePrefix, titleSuffix = msi.titleSuffix,
        filmLabel = msi.filmLabel, otherVenueLocations = msi.otherVenueLocations)
    case PolishVenue.Biletyna(cinema, page) =>
      new BiletynaClient(bnFetch, page, cinema, nationalFeed = Some(biletynaNationalFeed))
    case PolishVenue.BiletynaHalls(cinema, halls) =>
      new MultiListingScraper(cinema, halls.map((page, hall) =>
        new BiletynaClient(bnFetch, page, cinema, room = Some(hall), nationalFeed = Some(biletynaNationalFeed))))
    case PolishVenue.SystemBiletowy(cinema, portal, institution, filmGroup) =>
      new SystemBiletowyClient(http, portal, cinema, titles = titles, filmGroups = filmGroup.toSet, institution = institution)
  }

  /** Every Polish venue's scraper, in `venues`' order — each page's scrape order. */
  val scrapers: Seq[CinemaScraper] = venues.map(scraper)

  /** The hosts of the ticketing platforms' venue pages read through an egress other than direct — a host that blocks the
   *  worker's IP — each by that egress: read off the venues' own data, so a venue routed there routes every other link
   *  of its host (its posters) the same way, with no list of hosts kept beside them. */
  val egressByHost: Map[String, HttpFetch] =
    venues.flatMap {
      case msi: PolishVenue.Msi if msi.route != Route.Direct => Seq(msi.baseUrl -> fetchOf(msi.route))
      case venue                                               => biletynaPages(venue).map(_.url -> bnFetch)
    }.flatMap { case (url, fetch) => CinemaScraper.hostsOf(url).map(_ -> fetch) }.toMap
}
