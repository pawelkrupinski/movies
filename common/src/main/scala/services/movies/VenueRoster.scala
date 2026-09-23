package services.movies

import models.{Cinema, CinemaShowing, Country, Source}
import play.api.Logger

/**
 * The venues one country's database may hold side-collection rows (`screenings`,
 * `movie_slots`) under, and the write guard that keeps every other venue out.
 *
 * Why a guard at the write seam: each country has its own database, but nothing in the
 * side-collection stores knew which country that was, so a process pointed at the wrong
 * one wrote there without complaint. That happened: local per-country worker runs with
 * `.env.local`'s `MONGODB_DB=kinowo` ran the German and UK pipelines against the Polish
 * corpus. Prod PL carried 60 `movie_slots` rows under 14 German/UK venues until the
 * [[RetiredVenueRows]] sweep's first run removed them (2026-09-23) — mostly on orphaned
 * German-titled film ids, but some on live Polish films (Odyseja, Minionki i
 * straszydła …), each of which had grown a German-titled variant card on the Polish site. `DatabaseOwner`
 * now refuses the wrong database at worker boot; this refuses the ROW, so a script or any
 * later path that bypasses the boot claim still cannot land a foreign venue.
 *
 * What the guard refuses is a WRITE — creating or rewriting a row whose venue is not on
 * the roster. It never deletes: a stored row the payload still names is kept as stored.
 * During a rolling deploy the old pod's roster lacks a venue the new pod has just added
 * and is already writing; if refusing that venue also pruned its rows, the old pod would
 * delete them on every film it touched. Clearing rows of an off-roster venue is
 * [[RetiredVenueRows]]' job, with its own grace period.
 */
sealed trait VenueRoster {
  import VenueRoster.venueOf

  /** Whether a row under `slotKey` may be written. */
  def admits(slotKey: String): Boolean

  /** The payload a film-wide `replaceFilm` may write: every admitted row as given, every
   *  refused row that is already `stored` kept exactly as stored (see the class doc), and
   *  every refused row that is not stored dropped. Refusals are logged. */
  def writable[A](store: String, filmId: String, stored: Map[String, A], payload: Map[String, A]): Map[String, A] = {
    val (admitted, refused) = payload.partition { case (k, _) => admits(k) }
    if (refused.isEmpty) payload
    else {
      warnRefused(store, filmId, refused.keySet)
      admitted ++ refused.keysIterator.flatMap(k => stored.get(k).map(k -> _))
    }
  }

  /** Whether a single-row write under `slotKey` may go ahead; a refusal is logged. */
  def admitsWrite(store: String, filmId: String, slotKey: String): Boolean =
    admits(slotKey) || { warnRefused(store, filmId, Set(slotKey)); false }

  private def warnRefused(store: String, filmId: String, slotKeys: Set[String]): Unit =
    VenueRoster.logger.warn(s"[$store] refused to write $filmId under venue(s) outside this country's roster: " +
      slotKeys.toSeq.map(venueOf).distinct.sorted.mkString(", "))
}

object VenueRoster {

  private val logger = Logger(getClass)

  /** Every venue: for a store that is not scoped to one country (tests, web, dev tools). */
  case object Unrestricted extends VenueRoster {
    def admits(slotKey: String): Boolean = true
  }

  /** Exactly these venue names. */
  final case class Only(venues: Set[String]) extends VenueRoster {
    def admits(slotKey: String): Boolean = venues.contains(venueOf(slotKey))
  }

  def of(country: Country): VenueRoster = Only(venuesOf(country))

  /** The venue names a country's database may hold rows under — its own cities' venues,
   *  plus every `Source` that is not a physical venue (the chain-detail slots,
   *  TMDB/IMDb/Filmweb), which sit in no city at all. */
  def venuesOf(country: Country): Set[String] =
    country.cities.iterator.flatMap(_.cinemas).map(_.displayName).toSet ++ nonVenueSources

  private lazy val nonVenueSources: Set[String] = {
    val venues = Cinema.all.toSet
    Source.all.filterNot {
      case cinema: Cinema => venues.contains(cinema)
      case _              => false
    }.map(_.displayName).toSet
  }

  /** The venue a slot key is filed under: the `cinema.displayName` half of
   *  `"<cinema>␟<titleKey>"`, or the whole key for a legacy bare-cinema slot. */
  def venueOf(slotKey: String): String = slotKey.takeWhile(_ != CinemaShowing.Separator)
}
