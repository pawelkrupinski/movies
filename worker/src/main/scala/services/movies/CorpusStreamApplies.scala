package services.movies

import models.MovieRecord
import models.{CacheKey, FilmId, LeanRecords, StoredMovieRecord}

/**
 * The [[CorpusMirror]]'s change-stream applies: each upsert, venue-slots change and delete another writer made to the
 * store, applied to the resident corpus through its funnels the moment the stream delivers it, fenced against this
 * cache's own writes ([[FilmWriteFence]]). `logger` is the cache's, which the mirror has always logged as.
 */
private[movies] final class CorpusStreamApplies(corpus: MirroredCorpus, logger: play.api.Logger) {
  import corpus.{changed, evict, held as get, index as corpusIndex, keyOf, shape as residentShape, store, touch, writeFence}

  /** Apply one out-of-band upsert from the change stream to the in-memory cache.
   *  Mirrors a single row of `rehydrate` — a direct `positive.put`, bypassing
   *  the identity-gate `put` (Mongo is already the source of truth here, no
   *  re-folding needed).
   *
   *  …unless this cache has written the film since the stream re-read it (`mark`, see
   *  [[FilmWriteFence]]): that read is older than the resident row, and storing it rolled
   *  the write back until the write's own event re-read the film. That event still comes,
   *  so skipping the older read loses nothing. */
  private[services] def applyUpsert(r: StoredMovieRecord, mark: Long): Unit = {
    val key = keyOf(r)
    val applied = writeFence.ifUndisturbed(r.id.value, mark) {
      // A retitle arriving from another writer: the id's previous key entry is stale.
      corpusIndex.keyOf(r.id).filter(_ != key).foreach(evict)
      val held   = get(key)
      val cached = held.fold(residentShape.of(r.record))(residentShape.keeping(_, r.record))
      // The echo of a write this cache made reads back what it holds already — kept as the very object, not replaced by
      // a copy that every reader then compares slot by slot and that lives, until the film's next write, long enough to
      // be promoted (worker-us).
      if (!(held.exists(_ eq cached) && corpusIndex.idOf(key).contains(r.id))) store(key, cached, r.id)
      // ...and it is not another writer's change.
      if (!held.exists(LeanRecords.equal(_, cached))) changed(r.id)
    }
    if (applied) touch()
    else logger.debug(s"MovieCache: skipped a change-stream read of '${key.cleanTitle}' taken before this cache's own write of it.")
  }

  /** Apply a change confined to some venues' showtimes from those venues alone: their slots
   *  stripped again, the rest of the resident row untouched — exactly what [[applyUpsert]] of the
   *  whole film would store. Declined, having stored nothing, when the film is not resident or a
   *  venue holds other slots than the resident ones (a slot came or went), and the caller hands
   *  over the whole film instead. Fenced like [[applyUpsert]]. */
  private[services] def applyVenueSlots(venues: VenueSlots, mark: Long): VenueVerdict = {
    import ChangeStreamMetrics.VenueDecline as Why
    corpusIndex.keyOf(venues.filmId).flatMap(key => get(key).map(key -> _)) match {
      case None => VenueVerdict.Declined(Why.CacheNotResident)
      case Some((_, resident)) if !holdsSameSlots(resident, venues) => VenueVerdict.Declined(Why.CacheSlotsDiffer)
      case Some((key, resident)) =>
        if (writeFence.ifUndisturbed(venues.filmId.value, mark)(storeVenueSlots(key, resident, venues))) touch()
        VenueVerdict.Applied
    }
  }

  /** The venues' slots replace the resident ones whole, read as a whole-film read reads them, so
   *  the row stored is that read's exactly when the venues hold the same slots it does. */
  private def holdsSameSlots(resident: MovieRecord, venues: VenueSlots): Boolean =
    venues.atCinemas.forall { case (cinema, slots) =>
      resident.data.keysIterator.collect { case s @ models.CinemaShowing(`cinema`, _) => s }.toSet == slots.map(_._1).toSet
    }

  /** Only the venues' slots are stripped again, not the whole row as `forCache` would: a
   *  one-venue change to a wide film stripped thousands of slots — 4% of the US worker's CPU
   *  (JFR 2026-10-01). The slot set is unchanged ([[holdsSameSlots]]). Venues read back as the row holds them (the
   *  echo of this cache's own write) leave the row as it is. */
  private def storeVenueSlots(key: CacheKey, resident: MovieRecord, venues: VenueSlots): Unit = {
    val slots = venues.atCinemas.valuesIterator.flatten.map { case (source, slot) =>
      source -> residentShape.kept(resident.data.get(source), residentShape.slot(slot)) }.toSeq
    if (!slots.forall { case (source, slot) => resident.data.get(source).exists(_ eq slot) }) {
      store(key, resident.copy(data = resident.data ++ slots), venues.filmId)
      if (!slots.forall { case (source, slot) => resident.data.get(source).exists(LeanRecords.slotsEqual(_, slot)) })
        changed(venues.filmId)
    }
  }

  /** Apply an out-of-band DELETE from the change stream: drop the mirrored row whose
   *  source `_id` was removed (a fold/merge loser, an `UnscreenedCleanup` removal, a
   *  re-key's old id). The stream carries only the `_id`, so map it back to the CacheKey
   *  the row was stored under via `idFor`. Applying deletes incrementally is what lets the
   *  backstop rehydrate stop being the ONLY thing that catches them; a rare drifted /
   *  duplicate key that doesn't map is still swept by the backstop. */
  private[services] def applyDelete(id: FilmId): Unit = {
    corpusIndex.keyOf(id)
      .foreach { k =>
        evict(k)
        // An out-of-band Mongo delete arriving via the change stream — the mirror
        // of a fold/UnscreenedCleanup/re-key removal another writer made. The only
        // signal that a cache row vanished for a reason NOT originating on this node.
        RemovalAudit.filmRemoved("cache.applyDelete", id.value, reason = "change-stream-delete")
        touch()
        changed(id)
      }
  }
}
