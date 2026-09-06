package services.movies

import models.{Showtime, Source, SourceData}

/**
 * The showtimes half of the read-split: the rules for taking a film's showtimes OUT of its
 * `movies` document (`stripShowtimes`, `showtimesOf`), putting them BACK from `screenings`
 * on read (`stitch`, `reStitch`), diffing the two sides (`slotOps`, `changedSlots`), and the
 * one write rule the whole-record path follows (`applyFilm`).
 *
 * Kept apart from [[ScreeningsRepository]] because none of this is storage. The trait and
 * its two implementations only store; every rule about WHAT is stored lives here, once, so
 * `MongoMovieRepository`, the in-memory fake, the staging fold and the change stream all
 * read the same one — the drift this prevents is described on [[applyFilm]].
 */
object ScreeningsSplit {
  /** The cinema slots that carry showtimes, keyed by slot wire-key
   *  (`Source.displayName`). Tmdb/Imdb slots never have showtimes, so they're
   *  excluded. Pure — the write paths derive their screenings docs from this. */
  def showtimesOf(data: Map[Source, SourceData]): Map[String, Seq[Showtime]] =
    data.iterator.collect { case (s, sd) if sd.showtimes.nonEmpty => s.displayName -> sd.showtimes }.toMap

  /** Re-inject any slots STRIPPED for the cache (empty showtimes + a digest) from a
   *  film's current screenings — the full-replace delete-vector defense. A whole-record
   *  write from the (stripped) cache would otherwise `showtimesOf`-drop those slots and
   *  `replaceFilm`-delete their screenings; re-stitching keeps them. Slots carrying real
   *  showtimes pass through unchanged. Pure + unit-tested. */
  def reStitch(screenings: ScreeningsRepository, id: String, data: Map[Source, SourceData]): Map[Source, SourceData] =
    reStitchChecked(screenings, id, data).data

  /**
   * A re-stitched record, beside the read that re-stitched it.
   *
   * `stored` is the film's screenings AS STORED — the read `data` was refilled from. It
   * is carried out rather than dropped because the caller's next act is to write those
   * same rows back, and comparing against what is already there is the difference between
   * rewriting every slot of the film and writing nothing. Free: the read has happened
   * either way. Meaningless when `complete` is false — a read that did not see the film
   * cannot say what it holds.
   */
  case class ReStitched(data: Map[Source, SourceData], stored: Map[String, Seq[Showtime]], complete: Boolean)

  /**
   * Write one film's showtimes the way `MovieRepository.upsert` must, given the re-stitch that
   * read them. THE RULE LIVES HERE so the Mongo repository and the in-memory one cannot drift:
   * both call this, neither restates it. It drifted once — the fake wrote an unconditional
   * `replaceFilm` while production had grown two guards — which is the failure mode that lets a
   * spec pass against rules production does not follow.
   *
   * Three outcomes, and the middle one is the whole point of the split:
   *  - the re-stitch's READ FAILED: patch each slot individually. `replaceFilm` prunes every row
   *    the payload does not name, and a failed read under-reports the film, so a full replace here
   *    deletes the screenings it could not see.
   *  - the stored rows already equal the payload: write nothing. This is the outer of the two
   *    skip guards (`changedSlots` is the inner one) and it saves `replaceFilm` its own read.
   *  - otherwise replace, handing on the rows already read rather than making it read again.
   */
  def applyFilm(screenings: ScreeningsRepository, filmId: String,
                showtimes: Map[String, Seq[Showtime]], stitch: ReStitched): Unit =
    if (!stitch.complete) showtimes.foreach { case (slotKey, st) => screenings.upsertSlot(filmId, slotKey, st) }
    else if (showtimes != stitch.stored) screenings.replaceFilm(filmId, showtimes, Some(stitch.stored))

  /** [[reStitch]] plus whether the screenings read that fed it SAW the film. A
   *  `complete = false` means the stripped slots could not be refilled, so the result
   *  under-reports the film's showtimes and MUST NOT be handed to a full `replaceFilm` —
   *  its delete vector would erase every slot the read failed to return. */
  def reStitchChecked(screenings: ScreeningsRepository, id: String,
                      data: Map[Source, SourceData]): ReStitched = {
    val (scr, complete) = screenings.findForFilmChecked(id)
    val stitched = data.map {
      case (src, sd) if sd.showtimes.isEmpty && sd.showtimesDigest.isDefined =>
        src -> sd.copy(showtimes = scr.getOrElse(src.displayName, Seq.empty))
      case other => other
    }
    ReStitched(stitched, scr, complete)
  }

  /** The per-slot screening writes needed to turn `before`'s showtimes into
   *  `after`'s: `slotKey -> Some(showtimes)` to upsert, `slotKey -> None` to
   *  delete. Only slots whose showtimes actually changed appear — so a
   *  metadata-only change yields an empty map (no screening write) and a
   *  showtimes-only change writes ONLY here. Pure + unit-tested. */
  def slotOps(before: Map[Source, SourceData], after: Map[Source, SourceData]): Map[String, Option[Seq[Showtime]]] =
    (before.keySet ++ after.keySet).iterator.flatMap { s =>
      val bDigest = before.get(s).map(ShowtimesDigest.slotDigest).getOrElse(ShowtimesDigest.EmptyDigest)
      val aDigest = after.get(s).map(ShowtimesDigest.slotDigest).getOrElse(ShowtimesDigest.EmptyDigest)
      if (aDigest == bDigest) None
      else {
        val a = after.get(s).map(_.showtimes).getOrElse(Seq.empty)
        if (a.nonEmpty) Some(s.displayName -> Some(a))
        // An empty LIST is not evidence of an empty slot. A record resident in the cache has
        // been through `stripForCache`, which drops every list and keeps only the digest — so
        // "stripped" and "screening nothing" look identical here, and `putIfPresent` (every
        // rating refresh, every per-slot scrape update) hands us exactly those stripped
        // records. Deleting on that basis wipes the film's screenings for a cinema from a
        // record that never carried them.
        //
        // The digest is what tells the two apart. Delete only when it says the slot really is
        // empty — which also covers the slot vanishing from `after` entirely, a genuine
        // removal that must still delete. Otherwise we know the showtimes CHANGED but not
        // what to, so there is nothing this record can write: skip, and let the whole-record
        // path (`upsert`, which re-stitches from `screenings` first) carry the change.
        else if (aDigest == ShowtimesDigest.EmptyDigest) Some(s.displayName -> None)
        else None
      }
    }.toMap

  /** Movies-side view of a record's data with every slot's showtimes emptied — they
   *  live in `screenings` now. Used when WRITING `movies` under the read-split, so a
   *  showtime change doesn't rewrite the (fat) film document. */
  def stripShowtimes(data: Map[Source, SourceData]): Map[Source, SourceData] =
    data.view.mapValues(sd => if (sd.showtimes.isEmpty) sd else sd.copy(showtimes = Seq.empty)).toMap

  /** Re-inject each slot's showtimes from `screenings` (keyed by slot wire-key) when
   *  READING. `screenings` is AUTHORITATIVE: a slot with a screenings doc takes its
   *  showtimes; a slot without one has none. (`movies` no longer stores showtimes —
   *  the one-time embedded→screenings migration is complete.) Pure + unit-tested. */
  def stitch(data: Map[Source, SourceData], screenings: Map[String, Seq[Showtime]]): Map[Source, SourceData] =
    data.map { case (s, sd) =>
      s -> screenings.get(s.displayName).fold(if (sd.showtimes.isEmpty) sd else sd.copy(showtimes = Seq.empty))(st => sd.copy(showtimes = st))
    }

  /** The slots this film's write actually has to make — [[SlotKeyed.changedRows]] at this
   *  collection's payload type. Named here because `changedSlots` is what the write path reads
   *  as; the rule itself is shared with `movie_slots`, which has the identical shape. */
  def changedSlots(stored: Map[String, Seq[Showtime]], readComplete: Boolean,
                   incoming: Map[String, Seq[Showtime]]): Map[String, Seq[Showtime]] =
    SlotKeyed.changedRows(stored, readComplete, incoming)
}
