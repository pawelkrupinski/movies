package services.movies

import models.{MovieRecord, Source, SourceData}

import scala.collection.mutable

/**
 * In-memory `MovieRepository` for tests — full write-through semantics without
 * needing a real Mongo cluster. Implements the `MovieRepository` trait so it slots
 * in wherever the production cache expects a repository.
 *
 * Indexed by the same normalized documentId formula as the production repository
 * (`normalizer.sanitize(title)|year`), so case + diacritic + whitespace
 * variants of the same title collapse to one row exactly as they do in Mongo.
 *
 * Every upsert / delete is recorded in `upserts` / `deletes` in order so a
 * test can assert write-through behavior. Tests that don't care simply
 * ignore the buffers.
 */
class InMemoryMovieRepository(
  seed: Seq[(String, Option[Int], MovieRecord)] = Seq.empty,
  // Wire this to model the PRODUCTION storage split: showtimes stored in their own
  // collection keyed by film id, not inline in the record.
  //
  // Without it this fake cannot express the bug class that cost the most in 2026-07:
  // a merge or re-key is a RENAME, and the film's showtimes live under the OLD id, so
  // anything that writes the winner and deletes the loser destroys them. With showtimes
  // inline (the default) a fold unions the records and carries them for free, so every
  // merge/re-key spec passed while prod bled. Every routing decision below goes through
  // the SAME pure helpers `MongoMovieRepository` uses — `stripShowtimes`, `showtimesOf`,
  // `reStitch`, `stitch` — so the fake cannot drift from the real one.
  screenings: Option[ScreeningsRepository] = None,
  // The second half of the same split: the per-cinema `SourceData` themselves, out of
  // `movies.sourceData` and into `movie_slots`. Wire BOTH to get production's storage
  // shape — with only `screenings` the record still carries its cinema keys inline, so a
  // reader that never consults `movie_slots` still sees every cinema and the mid-migration
  // union (`SlotsRepository.merge`) is never exercised.
  slots: Option[SlotsRepository] = None,
  // The country whose rules derive a row's `_id`, mirroring `MongoMovieRepository`.
  // Last and defaulted so the positional constructions are unchanged; a spec that
  // is ABOUT country scoping passes its own instance instead of swapping a global.
  override val normalizer: services.movies.TitleNormalizer = services.movies.TitleNormalizer.deployment
) extends MovieRepository {
  // Fold titles with the rules the corpus was keyed under, not a process default.
  private given services.movies.TitleNormalizer = normalizer

  override def hasScreenings: Boolean = screenings.isDefined
  override def hasSlots:      Boolean = slots.isDefined

  private val store   = mutable.LinkedHashMap.empty[String, StoredMovieRecord]
  val upserts         = mutable.ListBuffer.empty[(String, Option[Int], MovieRecord)]
  val deletes         = mutable.ListBuffer.empty[(String, Option[Int])]

  // The enrichment cascade writes from several worker pools at once (TMDB,
  // IMDb, and the four *Ratings stages), so every store access is guarded by
  // this monitor. Production's `MongoMovieRepository` gets the same atomicity for
  // free (per-document `$set`/`$unset`, single-document writes are atomic in
  // Mongo); without it here the bare `mutable.Map` loses concurrent updates —
  // e.g. a rating stage's read-modify-write `updateIfPresent` interleaving with
  // a TMDB full-record `upsert` dropped the just-written `metacriticUrl`,
  // making the persisted row order-dependent even though the cache was correct.
  private val lock = new AnyRef

  // Stand-in for Mongo's change stream: any write (in-process or simulated
  // out-of-band) notifies the registered watchers, just like a real change
  // stream fires for every persisted edit — upserts AND deletes. Like the real
  // repository it fans ONE write path out to every registered listener (prod
  // attaches the cache AND the read-model projector); a single-watcher stub
  // would model only one and hide the multiplexing the real cursor now does.
  private val changes = new ChangeStreamFanout[StoredMovieRecord]("InMemoryMovieRepository")
  private def notifyWatcher(t: String, y: Option[Int], e: MovieRecord): Unit = {
    // Watchers get the record a READER would see, not the shrunken `movies` document —
    // the real change stream re-decodes the row and `decodeStitched` puts its slots and
    // showtimes back. A fake that dispatched the stripped record would have every
    // change-stream consumer (the cache, the read-model projector) see films with no
    // cinemas the moment the split was wired.
    val id = idOf(t, y)
    changes.dispatchUpsert(StoredMovieRecord.fromStorage(id, e.copy(data = stitchSides(
      id, e.data,
      slots.map(_.findAll()).getOrElse(Map.empty),
      screenings.map(_.findAll()).getOrElse(Map.empty)))))
  }

  private def stripFor(data: Map[Source, SourceData]): Map[Source, SourceData] =
    if (screenings.isDefined) ScreeningsRepository.stripShowtimes(data) else data

  override def watchChanges(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: String => Unit
  ): Option[AutoCloseable] = Some(changes.register(onUpsert, onDelete))

  seed.foreach { case (t, y, e) => store.put(idOf(t, y), StoredMovieRecord(t, y, e)) }

  def enabled: Boolean = true

  // Re-derive `title`/`year` from the stored `_id` + record via
  // `StoredMovieRecord.fromStorage` on read — EXACTLY as `MongoMovieRepository`'s
  // codec does (Mongo persists only `_id` + `sourceData`, dropping the title/
  // year columns). A faithful fake must do the same: returning the verbatim
  // upsert title instead hid two real bugs — title `_id`-drift (a row whose
  // display form moved away from its `_id`) and the `displayTitle`-based settle
  // non-determinism — because the fake's title could never drift and was always
  // deterministic where Mongo's re-derivation isn't. The fake differs from Mongo
  // only at the infra boundary (a HashMap, not BSON), never in read semantics.
  def findAll(): Seq[StoredMovieRecord] = lock.synchronized {
    val allScreenings = screenings.map(_.findAll()).getOrElse(Map.empty)
    val allSlots      = slots.map(_.findAll()).getOrElse(Map.empty)
    store.iterator.map { case (id, s) =>
      // Stitch FIRST, derive after — `fromStorage` reads the row's title off its cinema
      // slots, and for a migrated film those are in `movie_slots`, not in the stored record.
      // Same order as `MongoMovieRepository.stitchRow`, and for the same reason.
      StoredMovieRecord.fromStorage(id,
        s.record.copy(data = stitchSides(id, s.record.data, allSlots, allScreenings)))
    }.toSeq
  }

  /** Rebuild a row's `data` the way every production reader does: `movie_slots` UNIONED
   *  with whatever the `movies` document still embeds (neither is complete mid-migration),
   *  then showtimes re-injected from `screenings`. Same helpers, same order as
   *  `MongoMovieRepository.decodeStitched`. */
  private def stitchSides(
    id: String,
    embedded: Map[Source, SourceData],
    allSlots: Map[String, Map[String, SourceData]],
    allScreenings: Map[String, Map[String, Seq[models.Showtime]]]
  ): Map[Source, SourceData] = {
    val withSlots = slots.fold(embedded) { _ =>
      val stored = allSlots.getOrElse(id, Map.empty)
      if (stored.isEmpty) embedded else SlotsRepository.merge(embedded, stored)
    }
    screenings.fold(withSlots)(_ =>
      ScreeningsRepository.stitch(withSlots, allScreenings.getOrElse(id, Map.empty)))
  }

  def upsert(t: String, y: Option[Int], e: MovieRecord): Unit = lock.synchronized {
    val id = idOf(t, y)
    // Same order as `MongoMovieRepository.upsert`: re-stitch first (a record can arrive
    // stripped for cache residency, and `showtimesOf` would then drop showtimes that
    // `replaceFilm` proceeds to delete), then store the row WITHOUT showtimes and file
    // them under the film id.
    val restitched = screenings.fold(e.data)(ScreeningsRepository.reStitch(_, id, e.data))
    // Slots go FIRST, and the embedded map is dropped only once they have actually landed —
    // dropping it on a failed slot write is the one way this migration loses a film's
    // cinemas outright. An already-matching row counts as landed (see the same skip in
    // `MongoMovieRepository.upsert`, which exists so a showtime-only change doesn't churn
    // every slot of a film showing at 471 venues).
    val slotPayload = SlotsRepository.slotsOf(restitched)
    val slotsLanded = slots.exists(sl =>
      if (sl.findForFilm(id) == slotPayload) true else sl.replaceFilm(id, slotPayload))
    val dataForMovies =
      if (slotsLanded) Map.empty[Source, SourceData]
      else if (screenings.isEmpty) restitched
      else ScreeningsRepository.stripShowtimes(restitched)
    store.put(id, StoredMovieRecord(t, y, e.copy(data = dataForMovies)))
    screenings.foreach(_.replaceFilm(id, ScreeningsRepository.showtimesOf(restitched)))
    upserts.append((t, y, e))
    notifyWatcher(t, y, e)
  }

  /** Carry a film's screenings AND slots across a re-key / fold. Both stores move, or a
   *  fold keeps the showtimes and loses the cinema metadata that names them.
   *
   *  The read/verify/delete rule is `SideCollectionMove`'s — the SAME object
   *  `MongoMovieRepository.moveFilm` calls, not a re-statement of it. This fake used to
   *  replace-and-delete with no verification at all, so every re-key spec passed against a
   *  move production performs far more carefully, and the one condition that made the real
   *  move destructive (an unreadable destination) could not be expressed here at all. */
  override def moveFilm(oldId: String, newId: String): Boolean =
    if (oldId == newId) true else lock.synchronized {
      val screeningsMoved = screenings.forall(s => SideCollectionMove.move[Seq[models.Showtime]](
        oldId, newId,
        read       = s.findForFilmChecked,
        replace    = (id, rows) => { s.replaceFilm(id, rows); true },
        deleteFilm = s.deleteFilm))
      val slotsMoved = slots.forall(sl => SideCollectionMove.move[SourceData](
        oldId, newId,
        read       = sl.findForFilmChecked,
        replace    = sl.replaceFilm,
        deleteFilm = sl.deleteFilm))
      screeningsMoved && slotsMoved
    }

  def updateIfPresent(t: String, y: Option[Int], before: MovieRecord, after: MovieRecord): Boolean = lock.synchronized {
    val id = idOf(t, y)
    store.get(id) match {
      case None => false
      case Some(stored) =>
        // Mirror production's `$set`/`$unset` semantics: only the fields where
        // `before` and `after` differ get overwritten; everything else keeps
        // whatever the current store has. Tests for the audit-clobber race
        // depend on this.
        // Under the split the deltas are routed exactly as `MongoMovieRepository` routes
        // them: showtime changes to `screenings`, slot changes to `movie_slots`, and the
        // `movies` patch carries NEITHER. Patching `sourceData` here would resurrect the
        // embedded map field by field and undo the shrink the split exists for — and,
        // because `upsert` leaves that map empty, would patch onto emptiness and store a
        // record holding only the slots this one call happened to touch.
        val showtimeOps = if (screenings.isDefined) ScreeningsRepository.slotOps(before.data, after.data)
                          else Map.empty[String, Option[Seq[models.Showtime]]]
        val slotOps     = if (slots.isDefined) SlotsRepository.slotOps(before.data, after.data)
                          else Map.empty[String, Option[SourceData]]
        val rawPatch = MovieRecordPatch.diff(before.copy(data = stripFor(before.data)),
                                             after.copy(data = stripFor(after.data)))
        val patch    = if (slots.isDefined) rawPatch.copy(data = Map.empty) else rawPatch
        // Nothing changed — skip the write + change notification, exactly as
        // MongoMovieRepository does (no pure-`updatedAt` no-op event).
        if (patch.isEmpty && showtimeOps.isEmpty && slotOps.isEmpty) true
        else {
          screenings.foreach(s => showtimeOps.foreach {
            case (k, Some(times)) => s.upsertSlot(id, k, times)
            case (k, None)        => s.deleteSlot(id, k)
          })
          slots.foreach(sl => slotOps.foreach {
            case (k, Some(sd)) => sl.upsertSlot(id, k, sd)
            case (k, None)     => sl.deleteSlot(id, k)
          })
          val merged = patch.applyTo(stored.record)
          store.put(id, StoredMovieRecord(t, y, merged))
          upserts.append((t, y, merged))
          notifyWatcher(t, y, merged)
          true
        }
    }
  }

  def delete(t: String, y: Option[Int]): Unit = lock.synchronized {
    val id = idOf(t, y)
    store.remove(id)
    screenings.foreach(_.deleteFilm(id))   // the cascade the real repository owns
    slots.foreach(_.deleteFilm(id))
    deletes.append((t, y))
    changes.dispatchDelete(id)
  }

  def deleteById(id: String): Unit = lock.synchronized {
    if (store.remove(id).isDefined) {
      screenings.foreach(_.deleteFilm(id))
      slots.foreach(_.deleteFilm(id))
      deletes.append((id, None))
      changes.dispatchDelete(id)
    }
  }

  def close(): Unit = ()

  /** Out-of-band write straight to the store, bypassing `upsert`'s split routing AND the
   *  change notification.
   *
   *  It is the only way to produce a film whose cinemas are still EMBEDDED in the `movies`
   *  document — the state of every row nothing has rewritten since the slots split landed.
   *  `upsert` cannot make one, because it moves the slots out and empties the embedded map
   *  by design. A running cache is deliberately NOT notified: the point of the state is a
   *  cache whose view disagrees with the store. */
  def putEmbeddedOutOfBand(t: String, y: Option[Int], e: MovieRecord): Unit = lock.synchronized {
    store.put(idOf(t, y), StoredMovieRecord(t, y, e)); ()
  }

  /** Out-of-band edit: drop the `filmwebUrl` + `filmwebRating` for the row.
   *  Used by tests that simulate `FilmwebUrlAudit` mutating Mongo while a
   *  running cache holds stale values. */
  def dropFilmwebUrl(t: String, y: Option[Int]): Unit = {
    val id = idOf(t, y)
    store.get(id).foreach { s =>
      val updated = s.record.copy(filmwebUrl = None, filmwebRating = None)
      store.put(id, s.copy(record = updated))
      notifyWatcher(t, y, updated)
    }
  }

  private def idOf(t: String, y: Option[Int]): String = StoredMovieRecord.idFor(t, y)
}
