package services.movies

import play.api.Logger

/**
 * What one sweep of the side collections removed: rows per collection, and the distinct
 * film ids they were filed under. See [[StrandedSideRows.sweep]].
 */
final case class StrandedSideRows(screenings: Long, slots: Long, filmIds: Set[String]) {
  def rows: Long = screenings + slots
}

/**
 * Removing side-collection rows (`screenings`, `movie_slots`) whose film no longer has a
 * `movies` document — one rule shared by [[MongoMovieRepository]] and the in-memory fake,
 * above the trait seam for the same reason [[SideCollectionMove]] is: deciding what is
 * stranded is business logic, and a fake that decided it differently would let the
 * cleanup's specs pass against a rule production does not follow.
 *
 * Why the rows exist at all: every delete and merge now carries a film's side rows with
 * it (`delete` cascades, `moveFilm` copies-verifies-deletes), but the ones from before
 * that did not, and they never expire on their own. Measured on 2026-09-07: UK held 376
 * `screenings` rows under 18 film ids with no document (169 of them with FUTURE showtimes,
 * `startrekivthevoyagehome40thann|1986` last written 07-28), DE 29. The worker's
 * served-films census counted every one of them as a film it serves, the web could serve
 * none, and the invariant `FilmIdentityInvariantsSpec` proves in memory — side rows ⊆ live
 * ids — did not hold in the store.
 *
 * The rule, and what each step refuses:
 *
 *  1. Read the side collections' DISTINCT film ids first — a `$group`, never the rows.
 *     A store whose read failed is skipped entirely: its emptiness is "could not tell",
 *     not "nothing there", and nothing is deleted on its behalf.
 *  2. Then read the live `_id` set. It must be COMPLETE — a scan that stopped short would
 *     convict every film past the page it died on, which is the shape that has already
 *     cost this repository a 129-film outage. Incomplete ⇒ nothing is deleted.
 *  3. An EMPTY live set is refused too. A corpus with no documents is not something a
 *     cleanup should draw conclusions from (see `BackfillReadModel`, 2026-08-10: a
 *     prune on an empty read wiped live screenings); an empty `movies` collection has
 *     nothing that can serve the side rows anyway, so leaving them costs nothing.
 *  4. Stranded = side ids − live ids; delete per store with one `$in`.
 *
 * The order of 1 and 2 is load-bearing. `upsert` writes a NEW film's slots before its
 * `movies` document, so a sweep that read the corpus first could see the slots land after
 * a scan that (correctly) did not contain the film, and delete them from under a document
 * written milliseconds later. Reading the side ids first means a film is only stranded if
 * its rows existed BEFORE a corpus scan that still did not find it — a write straddling
 * the whole scan, not a normal upsert.
 */
object StrandedSideRows {

  val none: StrandedSideRows = StrandedSideRows(0, 0, Set.empty)

  private val logger = Logger(getClass)

  /** `liveIds` is the complete set of `movies` `_id`s, or `None` when the scan did not
   *  finish. A repository with neither side collection wired passes `None` for both
   *  stores and gets [[none]] back without scanning. */
  def sweep(
    screenings: Option[SlotKeyedRows],
    slots:      Option[SlotKeyedRows],
    liveIds:    () => Option[Set[String]]
  ): StrandedSideRows = {
    val (screeningFilms, screeningsRead) = filmIdsOf(screenings)
    val (slotFilms, slotsRead)           = filmIdsOf(slots)
    val candidates = screeningFilms ++ slotFilms
    if (!screeningsRead && !slotsRead) {
      logger.warn("Stranded side rows: neither side collection could be read — nothing removed.")
      none
    } else if (candidates.isEmpty) none   // no side rows at all; not worth a corpus scan
    else liveIds() match {
      case None =>
        logger.warn(s"Stranded side rows: the movies scan was INCOMPLETE — refusing to treat any of the " +
          s"${candidates.size} film(s) with side rows as stranded.")
        none
      case Some(live) if live.isEmpty =>
        logger.warn(s"Stranded side rows: the movies collection read as EMPTY — refusing to treat the " +
          s"${candidates.size} film(s) with side rows as stranded.")
        none
      case Some(live) =>
        val stranded = candidates -- live
        if (stranded.isEmpty) {
          logger.info(s"Stranded side rows: none — every one of the ${candidates.size} film(s) with side rows is live.")
          none
        } else {
          val screeningsDeleted = deleteFrom(screenings, screeningsRead, stranded & screeningFilms)
          val slotsDeleted      = deleteFrom(slots, slotsRead, stranded & slotFilms)
          logger.info(s"Stranded side rows: removed $screeningsDeleted screenings row(s) and $slotsDeleted movie_slots " +
            s"row(s) filed under ${stranded.size} film id(s) with no movies document.")
          RemovalAudit.strandedSideRowsRemoved("movies.deleteStrandedSideRows", screeningsDeleted, slotsDeleted, stranded)
          StrandedSideRows(screeningsDeleted, slotsDeleted, stranded)
        }
    }
  }

  private def filmIdsOf(store: Option[SlotKeyedRows]): (Set[String], Boolean) =
    store.fold((Set.empty[String], true))(_.filmIdsChecked())

  private def deleteFrom(store: Option[SlotKeyedRows], read: Boolean, filmIds: Set[String]): Long =
    if (read && filmIds.nonEmpty) store.fold(0L)(_.deleteFilms(filmIds)) else 0L
}
