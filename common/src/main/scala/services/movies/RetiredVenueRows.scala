package services.movies

import models.{Cinema, CinemaShowing, Country, Source}
import play.api.Logger

/**
 * What one sweep of retired-venue rows removed: rows per side collection, and the rows
 * removed per retired venue name. See [[RetiredVenueRows.sweep]].
 */
final case class RetiredVenueRows(screenings: Long, slots: Long, venues: Map[String, Long]) {
  def rows: Long = screenings + slots
}

/**
 * Removing side-collection rows (`screenings`, `movie_slots`) filed under a venue the
 * country's roster no longer lists.
 *
 * Why the rows exist: dropping a cinema from the roster stops it being scraped, and the
 * read path already ignores its rows (`SlotsRepository.stitch` drops a wire key that no
 * longer names a `Source`), but nothing ever DELETES them. Neither existing sweep can:
 * [[StrandedSideRows]] convicts a row by its FILM (no `movies` document) or its missing
 * slot twin, and the film here is alive — it just has a venue slot that points nowhere.
 * `UnscreenedCleanup` acts on whole films whose cinemas are all gone. So after "Kino
 * Etiuda OBK" left the roster (39e04cdd2) prod PL kept 3 films and 8 future showtimes
 * under it indefinitely, counted by every corpus-side census that reads the side
 * collections raw.
 *
 * The rule, and what each step refuses:
 *
 *  1. The roster is the database's OWN country's venues ([[rosterOf]]) — each country
 *     has its own database, claimed at boot by `DatabaseOwner`, so a foreign venue's row
 *     in it is as unservable as a retired one. Plus every `Source` that is not a physical
 *     venue (the chain-detail slots, TMDB/IMDb/Filmweb), which sit in no city at all.
 *  2. A row's venue is the `cinema.displayName` half of its slot key — everything before
 *     the `␟` in `"<cinema>␟<titleKey>"`, or the whole key for a legacy bare-cinema slot.
 *     Compared by EQUALITY, never prefix: "Kino Etiuda OBK" is retired while "Kino Etiuda"
 *     is live, and a `startsWith` would read one as the other.
 *  3. An EMPTY roster removes nothing — that is a roster that failed to load, not a
 *     country with no cinemas.
 *  4. A roster that would retire more than [[MaxRetiredShare]] of the venues the store
 *     holds removes nothing either: a half-loaded data-driven roster (`GermanRoster`,
 *     `UsRoster`, `SpanishRoster` read resource files) or a mis-wired country would
 *     otherwise convict most of a live corpus. Real removals are a handful of venues at a
 *     time; a bulk one can be split across deploys.
 *  5. A store whose id read failed is skipped — "could not read the ids" is not "no ids".
 *
 * Idempotent: once a venue's rows are gone a second sweep finds nothing, and no retired
 * venue is scraped, so nothing writes them back.
 */
object RetiredVenueRows {

  val none: RetiredVenueRows = RetiredVenueRows(0, 0, Map.empty)

  /** The largest share of stored venues one sweep may retire before it refuses outright. */
  val MaxRetiredShare: Double = 0.2

  private val logger = Logger(getClass)

  /** The venue names a country's database may hold rows under — its own cities' venues,
   *  plus every `Source` that is not a physical venue (see step 1). */
  def rosterOf(country: Country): Set[String] =
    country.cities.iterator.flatMap(_.cinemas).map(_.displayName).toSet ++ nonVenueSources

  private lazy val nonVenueSources: Set[String] = {
    val venues = Cinema.all.toSet
    Source.all.filterNot {
      case cinema: Cinema => venues.contains(cinema)
      case _              => false
    }.map(_.displayName).toSet
  }

  /** The venue a side-collection row is filed under, from its composite `_id`. */
  def venueOf(rowId: String): String =
    rowId.drop(SlotKeyed.filmIdOf(rowId).length + 1).takeWhile(_ != CinemaShowing.Separator)

  def sweep(screenings: Option[SlotKeyedRows], slots: Option[SlotKeyedRows], roster: Set[String]): RetiredVenueRows = {
    val screeningIds = idsOf(screenings, "screenings")
    val slotIds      = idsOf(slots, "movie_slots")
    val stored       = (screeningIds.toSeq.flatten ++ slotIds.toSeq.flatten).map(venueOf).toSet
    val retired      = stored -- roster
    if (roster.isEmpty) {
      logger.warn("Retired-venue rows: the roster is EMPTY — refusing to treat any venue as retired.")
      none
    } else if (retired.isEmpty) none
    else if (retired.size > stored.size * MaxRetiredShare) {
      logger.warn(s"Retired-venue rows: the roster would retire ${retired.size} of the ${stored.size} venue(s) the side " +
        s"collections hold (over ${(MaxRetiredShare * 100).round}%) — refusing, a roster that short is more likely " +
        s"half-loaded than pruned. Venues: ${retired.toSeq.sorted.take(20).mkString(", ")}")
      none
    } else {
      val screeningRows     = screeningIds.fold(Set.empty[String])(_.filter(id => retired(venueOf(id))))
      val slotRows          = slotIds.fold(Set.empty[String])(_.filter(id => retired(venueOf(id))))
      val screeningsDeleted = if (screeningRows.isEmpty) 0L else screenings.fold(0L)(_.deleteRows(screeningRows))
      val slotsDeleted      = if (slotRows.isEmpty) 0L else slots.fold(0L)(_.deleteRows(slotRows))
      if (screeningsDeleted + slotsDeleted == 0) none
      else {
        val perVenue = (screeningRows.toSeq ++ slotRows.toSeq).groupMapReduce(venueOf)(_ => 1L)(_ + _)
        RemovalAudit.retiredVenueRowsRemoved("side-rows.retiredVenues", screeningsDeleted, slotsDeleted, perVenue)
        RetiredVenueRows(screeningsDeleted, slotsDeleted, perVenue)
      }
    }
  }

  /** A store's row ids, or None when it is not wired or its read failed (logged). */
  private def idsOf(store: Option[SlotKeyedRows], label: String): Option[Set[String]] =
    store.flatMap { s =>
      val (ids, read) = s.rowIdsChecked()
      if (!read) logger.warn(s"Retired-venue rows: the $label row ids could not be read — no $label row removed.")
      Option.when(read)(ids)
    }
}
