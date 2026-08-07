package services.movies

import models.{MovieRecord, Source, SourceData}
import play.api.Logging
import services.staging.StagingRepository


/**
 * Sends the cinemas of a row's SECOND film back to staging, so each film ends up
 * with a record of its own.
 *
 * Two unrelated films released under one Polish title share a row, because the row
 * is keyed by title. `FilmCanonicalizer.clusterByFilm` would keep them apart, but
 * it splits ROWS by tmdbId and a row holding both films has only one — so whatever
 * it resolves to, the cinemas showing the other film are served the wrong record.
 * "Joanna d'Arc" holds Besson's 1999 film and Pálmason's 2025 one and resolves to
 * neither; "Obcy" serves Ozon's "L'étranger" to the one cinema screening Brandt
 * Andersen's film.
 *
 * Rather than build a second way to create a record, this undoes the merge: the
 * stray slots go back to `pending_movies`, exactly as a newcomer's scrape diverts
 * them, and the ordinary staging path takes it from there — each row resolves on
 * its OWN hints (`StagingSteps.hintGroupKey` already groups by director and
 * original title, so they resolve apart), and the fold gives each cluster its own
 * `movies` row. Dropping the slots here also means the next scrape tick sees the
 * cinema as untouched on the old row, so nothing is left pointing at both.
 *
 * Runs as part of the SETTLE pass, alongside the other consolidations that bring
 * the corpus to its steady state — a mixed row is a consolidation problem, and
 * settle is where the cache already re-keys and merges. It is also the backstop:
 * `MovieCache.recordCinemaScrape` now refuses the merge in the first place when a
 * cinema's listing contradicts the row, so this only has to clear rows that merged
 * before that check existed, or whose contradiction surfaced later in a detail
 * fetch rather than in the listing.
 *
 * Deliberately conservative — see `MixedFilmDetector`. Only a positive
 * contradiction between what two cinemas PUBLISHED counts, never one cinema
 * merely describing the film less fully than another, and never anything derived
 * from the resolution being checked.
 */
class MixedFilmSplitter(cache: MovieCache, staging: StagingRepository) extends Logging {

  /** Re-divert every stray slot in the corpus. Returns how many were sent back.
   *
   *  Idempotent: a split row no longer holds the slots that made it mixed, so a
   *  second pass over the same corpus finds nothing. Safe to run on a cadence. */
  def splitMixedRows(): Int = {
    val work = cache.snapshot().flatMap { entry =>
      val strays = MixedFilmDetector.strays(entry.record, cache.normalizer)
      if (strays.isEmpty) None else Some((entry, strays))
    }

    work.foldLeft(0) { case (moved, (entry, strays)) => moved + split(entry, strays) }
  }

  /** Send one row's stray slots back to staging. Returns how many moved — 0 when the
   *  row's stored copy could not be read, which defers the split rather than staging
   *  a cinema whose showtimes we could not see. */
  private def split(entry: StoredMovieRecord, strays: Seq[(Source, SourceData)]): Int = {
    val key = cache.keyOf(entry.title, entry.year)
    // Re-read the row from storage before moving anything off it. `snapshot()` is the
    // CACHE-RESIDENT view, and under the read-split that view has been through
    // `ShowtimesDigest.stripForCache` — every slot on it holds `showtimes = Nil`, because
    // the lists live in `screenings` under the film's id. Detection is happy with that
    // (original title, runtime and year all survive the strip), but STAGING a slot from it
    // sends the cinema to `pending_movies` with an empty board: the fold then gives it a
    // row with no showtimes, the film drops off the site until that cinema is scraped
    // again, and the re-scrape re-attaches the listing to the row it just left — mixed
    // again for the next settle to split. Measured on prod 2026-08-06 as
    // `ktoscalkiemobcy|2024`, 0 slots and 0 screenings, on a 30-minute cycle.
    //
    // A FAILED read is not an empty film. Staging on the strength of one writes that empty
    // board over a real one, so leave the row mixed and split it on a pass that can read.
    val (restitched, readOk) = cache.restitchedChecked(key)
    // ABSENT counts as unreadable here, not as "this film has no cinemas". `readOk` only
    // says the read completed; it can complete and find nothing, because the id is
    // re-derived from the resident row's DISPLAY title and that can drift from the
    // persisted `_id` (`StoredMovieRecord.idOf` documents the drift, and
    // `MovieCache.rehydrate` migrates rows it catches). On that path `storedSlots` is empty,
    // the `getOrElse` below falls back to the resident slot, and we stage the cinema with
    // the empty board this guard exists to prevent — the same `ktoscalkiemobcy|2024` shape,
    // reached through the absent branch instead of the failed one.
    if (!readOk || restitched.isEmpty) {
      logger.warn(s"Mixed-film split deferred for '${entry.title}' (${entry.year.getOrElse("?")}): " +
        s"its stored row could not be ${if (readOk) "found" else "read"}, so the stray cinema's " +
        "showtimes are unknown — the next settle splits it once the row resolves.")
      return 0
    }
    val storedSlots = restitched.map(_.data).getOrElse(Map.empty)

    strays.foreach { case (source, slot) =>
      Source.cinemaOf(source).foreach { cinema =>
        val title = slot.title.filter(_.trim.nonEmpty).getOrElse(entry.title)
        // The STORED slot carries this cinema's showtimes; the resident one carries none.
        val staged = storedSlots.getOrElse(source, slot)
        staging.upsert(cinema, title, slot.releaseYear, MovieRecord(
          searchTitle = Some(cache.normalizer.apiQuery(cache.normalizer.recase(title))),
          data        = Map(source -> staged)))
        logger.info(s"Mixed-film split: '${entry.title}' (${entry.year.getOrElse("?")}) — " +
          s"${cinema.displayName} screens a different film " +
          s"[director: ${slot.director.mkString(", ")}; original: ${slot.originalTitle.getOrElse("—")}] " +
          s"→ re-diverted to staging as '$title' (${slot.releaseYear.getOrElse("?")}) " +
          s"with ${ShowtimesDigest.slotShowtimeCount(staged)} showtime(s).")
      }
    }
    // Drop them from the row in ONE update so the row is never observed holding
    // a slot staging has already taken over.
    val strayKeys = strays.map(_._1).toSet
    cache.putIfPresent(key, r => r.copy(data = r.data -- strayKeys))
    strays.size
  }

}
