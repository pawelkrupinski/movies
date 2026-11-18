package services.cinemas.pl

import models.Cinema
import services.cinemas.common.CinemaScraper
import services.titles.TitleNormalizer
import tools.HttpFetch

import java.time.LocalDate

/**
 * One Polish venue as its town's [[PolishVenueRegistry]] declares it: the cinema and the source its scraper reads,
 * and nothing that is built. `services.cinemas.PolishScraperCatalog` builds every venue's scraper from these, binding
 * the routes, the clock, the chains' detail caches and the title rules — so a venue on a platform listed here is one
 * line of data in its town's registry, and a venue with a client of its own is one [[PolishVenue.OwnClient]].
 */
sealed trait PolishVenue

object PolishVenue {
  /** A Cinema City venue, by the chain's cinema id. */
  final case class CinemaCity(cinemaId: String, cinema: Cinema) extends PolishVenue
  /** A Multikino venue, by the chain's cinema id. */
  final case class Multikino(cinemaId: String, cinema: Cinema) extends PolishVenue
  /** A Helios venue ([[HeliosNuxt]] names each one). */
  final case class Helios(config: HeliosCinema) extends PolishVenue
  /** An organiser listing on bilety24.pl itself. */
  final case class Bilety24Organizer(organizerUrl: String, cinema: Cinema) extends PolishVenue
  /** A venue on its own `<venue>.bilety24.pl` subdomain. */
  final case class Bilety24Subdomain(cinema: Cinema, url: String) extends PolishVenue
  /** An ekobilet.pl venue, by its slug. */
  final case class Ekobilet(slug: String, cinema: Cinema) extends PolishVenue
  /** A NoveKino venue, by its slug. */
  final case class NoveKino(slug: String, cinema: Cinema) extends PolishVenue
  /** Filmweb's seances, by Filmweb's internal cinema id. */
  final case class Filmweb(filmwebCinemaId: Int, cinema: Cinema) extends PolishVenue
  /** An MSI / VisualTicket portal: `mvcPath` / `titlePrefix` / `titleSuffix` / `filmLabel` / `otherVenueLocations` are
   *  the per-install quirks [[MsiClient]] documents; `route` the egress its host needs ([[Route]]). */
  final case class Msi(cinema: Cinema, baseUrl: String, mvcPath: String, titlePrefix: Option[String],
                       titleSuffix: Option[String], filmLabel: Option[String], otherVenueLocations: Set[String],
                       route: Route) extends PolishVenue
  /** A biletyna.pl place page. biletyna.pl 403s our datacenter IP (Cloudflare waiting-room), so every biletyna venue
   *  routes through the biletyna egress — the residential proxy in prod, the fixture fake in tests — and reads the
   *  one national event feed all of them share. */
  final case class Biletyna(cinema: Cinema, page: BiletynaPlacePage) extends PolishVenue
  /** One cinema whose halls are each on a biletyna place page of their own, every showtime tagged with its hall. */
  final case class BiletynaHalls(cinema: Cinema, halls: Seq[(BiletynaPlacePage, String)]) extends PolishVenue
  /** A systembiletowy.pl install, by portal base URL; `institution` scopes an instance that also sells someone else's
   *  events to the venue's own, `filmGroup` keeps a feed mixing films with a stage programme to the films' category
   *  (see [[SystemBiletowyClient]]). */
  final case class SystemBiletowy(cinema: Cinema, portal: VisualSoftPortal, institution: Option[Institution],
                                  filmGroup: Option[EventCategory]) extends PolishVenue
  /** A venue with a client of its own, built from what every such client shares ([[PolishScraperKit]]). */
  final case class OwnClient(build: PolishScraperKit => CinemaScraper) extends PolishVenue

  /** The egress a platform venue's host needs, when it is not the direct one. */
  enum Route {
    /** The worker's own IP. */
    case Direct
    /** Zyte's residential egress, for a host whose origin firewall blocks both the worker's IP and the Decodo proxy
     *  (see `services.cinemas.CinemaEgress.zyte`). */
    case Zyte
  }
}

/** What a venue with a client of its own ([[PolishVenue.OwnClient]]) is built from: the direct route, biletyna's proxied
 *  route, today in Poland — asked afresh at every read, so a scraper built at boot still plans from the day it scrapes
 *  on — and the title rules. */
final class PolishScraperKit(val http: HttpFetch, val biletynaFetch: HttpFetch, todayInPoland: () => LocalDate,
                             val titles: TitleNormalizer) {
  def today: LocalDate = todayInPoland()
}

/**
 * The Polish venues wired near one town: one registry per town (`services.cinemas.pl.venues`, next to the Polish
 * clients), which `pl.venues.PolishTowns` lists and `services.cinemas.PolishScraperCatalog` builds. A town is only where
 * a venue is WIRED: the page a venue is listed on — its own town, a cluster of small towns, or a major city — is
 * `models.PolishPages`, and `CinemaScraperCatalog.byCity` files every scraper under that page, whichever registry
 * declares it.
 *
 * Three tiers, each scraped after the one before across every town (`PolishTowns` keeps each tier's town order): the
 * town's own venues; the catchment cinemas and few in-city venues a Filmweb sweep found near it, each by Filmweb
 * internal cinema id except the handful since moved to their own site; and the nearby towns the 2026-09 sweeps added
 * (Filmweb, bilety24, biletyna, the chains' own branch lists), every venue about an hour's drive from the town.
 *
 * A registry holds data only ([[PolishVenue]]); the helpers below name each platform.
 */
trait PolishVenueRegistry {
  /** The town's own venues. */
  def venues: Seq[PolishVenue]
  /** The cinemas a Filmweb sweep found near the town. */
  def filmwebCatchment: Seq[PolishVenue] = Nil
  /** The nearby towns' venues the 2026-09 sweeps added. */
  def nearbyTowns: Seq[PolishVenue] = Nil

  protected final def cinemaCity(cinemaId: String, cinema: Cinema): PolishVenue = PolishVenue.CinemaCity(cinemaId, cinema)
  protected final def multikino(cinemaId: String, cinema: Cinema): PolishVenue  = PolishVenue.Multikino(cinemaId, cinema)
  protected final def helios(config: HeliosCinema): PolishVenue                  = PolishVenue.Helios(config)
  protected final def bilety24(organizerUrl: String, cinema: Cinema): PolishVenue =
    PolishVenue.Bilety24Organizer(organizerUrl, cinema)
  protected final def bilety24Subdomain(cinema: Cinema, url: String): PolishVenue = PolishVenue.Bilety24Subdomain(cinema, url)
  protected final def ekobilet(slug: String, cinema: Cinema): PolishVenue = PolishVenue.Ekobilet(slug, cinema)
  protected final def noveKino(slug: String, cinema: Cinema): PolishVenue = PolishVenue.NoveKino(slug, cinema)
  protected final def filmweb(filmwebCinemaId: Int, cinema: Cinema): PolishVenue = PolishVenue.Filmweb(filmwebCinemaId, cinema)
  protected final def msi(cinema:              Cinema,
                          baseUrl:             String,
                          mvcPath:             String         = MsiClient.DefaultMvcPath,
                          titlePrefix:         Option[String] = None,
                          titleSuffix:         Option[String] = None,
                          filmLabel:           Option[String] = None,
                          otherVenueLocations: Set[String]    = Set.empty,
                          route:               PolishVenue.Route = PolishVenue.Route.Direct): PolishVenue =
    PolishVenue.Msi(cinema, baseUrl, mvcPath, titlePrefix, titleSuffix, filmLabel, otherVenueLocations, route)
  protected final def biletyna(cinema: Cinema, page: String): PolishVenue = PolishVenue.Biletyna(cinema, BiletynaPlacePage(page))
  protected final def biletynaHalls(cinema: Cinema, halls: Seq[(String, String)]): PolishVenue =
    PolishVenue.BiletynaHalls(cinema, halls.map { case (page, hall) => BiletynaPlacePage(page) -> hall })
  protected final def systemBiletowy(cinema: Cinema, portal: String, institution: Option[Institution] = None,
                                     filmGroup: Option[EventCategory] = None): PolishVenue =
    PolishVenue.SystemBiletowy(cinema, VisualSoftPortal(portal), institution, filmGroup)
  protected final def own(build: PolishScraperKit => CinemaScraper): PolishVenue = PolishVenue.OwnClient(build)
}
