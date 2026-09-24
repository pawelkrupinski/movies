package services.movies

import play.api.Logger

import java.time.Instant
import scala.concurrent.duration._

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
 *  1. The roster is the database's OWN country's venues ([[VenueRoster.venuesOf]]) — each country
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
 *  5. A store whose id read failed stops the WHOLE sweep — "could not read the ids" is not
 *     "no ids", and sweeping the other store alone would judge rule 6 without the missing
 *     twin's stamp and could split a pair.
 *  6. A row written within the last [[Grace]] (24h, longer than any rollout) is left alone
 *     even when its venue is off the roster. During a rolling deploy the OLD pod's roster
 *     lacks a venue the NEW pod has just added and is already writing; its sweep must not
 *     delete those rows. A genuinely retired venue is never scraped again, so its rows only
 *     age and are swept on the first tick after the grace runs out. The grace is judged on
 *     the FRESHER of a row's `screenings` / `movie_slots` twins, so a sweep never deletes one
 *     twin and leaves the other.
 *
 * Idempotent: once a venue's rows are gone a second sweep finds nothing, and no retired
 * venue is scraped, so nothing writes them back.
 */
object RetiredVenueRows {

  val none: RetiredVenueRows = RetiredVenueRows(0, 0, Map.empty)

  /** How long a row must have gone unwritten before a retired venue's row may be swept. */
  val Grace: FiniteDuration = 24.hours

  /** The largest share of stored venues one sweep may retire before it refuses outright. */
  val MaxRetiredShare: Double = 0.2

  private val logger = Logger(getClass)

  /** The venue a side-collection row is filed under, from its composite `_id`. */
  def venueOf(rowId: String): String =
    VenueRoster.venueOf(SlotKeyed.slotKeyOf(rowId))

  def sweep(screenings: Option[SlotKeyedRows], slots: Option[SlotKeyedRows], roster: Set[String],
            now: Instant = Instant.now(), grace: FiniteDuration = Grace): RetiredVenueRows =
    if (roster.isEmpty) {
      logger.warn("Retired-venue rows: the roster is EMPTY — refusing to treat any venue as retired.")
      none
    } else (stampsOf(screenings, "screenings"), stampsOf(slots, "movie_slots")) match {
      case (Some(screeningIds), Some(slotIds)) => sweepRead(screenings, slots, screeningIds, slotIds, roster, now.minusMillis(grace.toMillis))
      case _                                   => none
    }

  private def sweepRead(screenings: Option[SlotKeyedRows], slots: Option[SlotKeyedRows],
                        screeningIds: Map[String, Instant], slotIds: Map[String, Instant],
                        roster: Set[String], cutoff: Instant): RetiredVenueRows = {
    val stored  = (screeningIds.keySet ++ slotIds.keySet).map(venueOf)
    val retired = stored -- roster
    // A row is as fresh as its freshest twin: `screenings` and `movie_slots` share row ids, and
    // judging each store's stamp alone split pairs (prod US 2026-09-23: 8 slot rows swept, their
    // recently rewritten screenings twins left twinless with 200 future showtimes).
    val freshest = (screeningIds.toSeq ++ slotIds.toSeq).groupMapReduce(_._1)(_._2)((a, b) => if (a.isAfter(b)) a else b)
    def sweepable(id: String): Boolean = retired(venueOf(id)) && freshest(id).isBefore(cutoff)
    if (retired.isEmpty) none
    else if (retired.size > stored.size * MaxRetiredShare) {
      logger.warn(s"Retired-venue rows: the roster would retire ${retired.size} of the ${stored.size} venue(s) the side " +
        s"collections hold (over ${(MaxRetiredShare * 100).round}%) — refusing, a roster that short is more likely " +
        s"half-loaded than pruned. Venues: ${retired.toSeq.sorted.take(20).mkString(", ")}")
      none
    } else {
      val screeningRows     = screeningIds.keySet.filter(sweepable)
      val slotRows          = slotIds.keySet.filter(sweepable)
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

  /** A store's row ids with their last write — empty when the store is not wired — or None
   *  when its read failed (logged). */
  private def stampsOf(store: Option[SlotKeyedRows], label: String): Option[Map[String, Instant]] =
    store.fold(Option(Map.empty[String, Instant])) { s =>
      val (ids, read) = s.rowWrittenAtChecked()
      if (!read) logger.warn(s"Retired-venue rows: the $label row ids could not be read — removing no row from either collection.")
      Option.when(read)(ids)
    }
}
