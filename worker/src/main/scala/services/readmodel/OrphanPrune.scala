package services.readmodel

import play.api.Logging

/** THE PRUNE of a [[ReadModelSweeper]] sweep whose source scan was COMPLETE: every card no live row projects to,
 *  every screenings row whose film is gone, and the venues a live card holds that its row no longer lists — each
 *  delete guarded on its own, off id-only reads, never payloads. An unread card or screening is never an orphan,
 *  and past the cap no unlisted venue is pruned at all.
 *
 *  Built by the sweeper over the projection it sweeps, and logging under the sweeper's `logger`: a prune reads as
 *  its sweep's, which it is. Every method expects the projection lock held. */
private[readmodel] final class OrphanPrune(projection: SweptProjection, override protected val logger: play.api.Logger)
    extends ContinuingSteps with Logging {
  import ReadModelProjectionMetrics.PruneReason
  private val reader  = projection.reader
  private val metrics = projection.metrics

  /** Caller holds the projection lock. The destructive half of a sweep whose scan was COMPLETE: prune off id-only
   *  projections — the prune reads ids/filmIds, never payloads. Each delete guarded: a read-model
   *  write THROWS on failure, and one refused delete must not skip every other orphan, the content
   *  slice, the catch-up and the sweep's metrics. */
  def prune(reproject: Boolean, kind: String, baseline: SweepBaseline, tally: SweepTally): Unit = {
    pruneOrphanCards(reproject, kind, baseline, tally)
    // Drop metadata cached for source rows that no longer exist. Only reachable on a
    // COMPLETE scan — on a truncated one `liveRowKeys` is partial and this would evict
    // live rows' metadata, costing a needless recompute each.
    projection.retainRows(tally.liveRowKeys)
    // A screenings read that failed prunes none: an unread row is not an orphan.
    baseline.screeningRefs.orElse(if (reproject) reader.findAllScreeningRefsChecked().answered else None).getOrElse {
      logger.warn(s"read-model $kind: the screening ids could not be read — no screening is pruned this tick."); Seq.empty
    }.iterator.filterNot(ref => tally.liveIds(ref.filmId)).foreach(pruneScreening(_, kind, tally))
    pruneUnlistedVenues(kind, baseline, tally)
  }

  /** Caller holds the projection lock. Prune every card no live row projects to.
   *
   *  The cards as they were BEFORE the scan, when that read was complete: a card written since
   *  (another process, during a rolling deploy) belongs to a row the scan may not have seen, and
   *  diffing a fresh read against `liveIds` pruned it as an orphan. Those cards are the next
   *  sweep's to judge; this one's heals are in `liveIds` either way. A card read that failed judges
   *  none: an unread card is not an orphan. */
  private def pruneOrphanCards(reproject: Boolean, kind: String, baseline: SweepBaseline, tally: SweepTally): Unit = {
    val cardsToJudge = (if (!reproject) baseline.cards else reader.findAllMovieIdsChecked().answered).getOrElse {
      logger.warn(s"read-model $kind: the card ids could not be read — no card is pruned this tick."); Seq.empty }
    cardsToJudge.iterator.filterNot(tally.liveIds).foreach { id =>
      val rowOfCard = id.takeWhile(_ != '~')
      continuing(s"read-model $kind: pruning card $id failed") {
        deleteFilm(id, if (tally.liveRowKeys(rowOfCard)) PruneReason.VariantGone else PruneReason.RowGone)
        tally.prunedFilms += 1
      }.getOrElse { tally.pruneFailed = true }
    }
  }

  /** Caller holds the projection lock. Delete one orphan screenings row, guarded like every prune write. */
  private def pruneScreening(ref: ScreeningRef, kind: String, tally: SweepTally): Unit =
    continuing(s"read-model $kind: pruning screening ${ref._id} failed") {
      projection.removeScreening(ref)
      tally.prunedScreenings += 1
    }.getOrElse { tally.pruneFailed = true }

  /** Caller holds the projection lock. Prune the venues a live card holds that its row no longer lists at all
   *  (`unlisted`): a removal the stream never applied, which no id check of the CARD can see — the
   *  card stays, its row does too, and only the content check ever rewrote it, up to a day later
   *  (us/chicago served Dracula: A Love Tale at a venue it had left for 22 hours, 2026-09-30).
   *
   *  A stream misses a removal now and then; one sweep finding a great many is far likelier a
   *  comparison gone wrong — the slots-only view disagreeing with the projection about some shape
   *  of row — and pruning on it would blank the served venues of a country. So past the cap the
   *  sweep prunes none of them and says so loudly; the content check still rewrites each row.
   *
   *  Most of what it finds is no miss at all: a removal the stream still holds (the DE/UK/US debounce
   *  keeps a film's re-read up to two minutes) or has queued behind this sweep's lock. Deleting those
   *  is harmless, but only the ones the stream does not then apply are missed removals — the
   *  sweep's verdict ([[SweepVerdict.reportUnlisted]]) tells them apart once the lock is let go. */
  private def pruneUnlistedVenues(kind: String, baseline: SweepBaseline, tally: SweepTally): Unit = {
    val unlisted = tally.unlisted
    val cap      = ReadModelSweeper.unlistedPruneCap(baseline.screeningRefs.fold(0)(_.size))
    if (unlisted.sizeIs > cap) {
      metrics.recordUnlistedVenues(unlisted.size, withheld = true)
      logger.error(s"read-model $kind sweep: ${unlisted.size} venue row(s) whose film no longer lists the venue, over the " +
        s"cap of $cap — pruning NONE; a disagreement between the slots-only view and the projection is likelier than " +
        s"that many missed removals: ${ReadModelSweeper.idsForLog(unlisted.map(_._id))}.")
    } else {
      unlisted.foreach(pruneScreening(_, kind, tally))
      tally.prunedUnlisted ++= unlisted
    }
  }

  // Only the card prune calls this — a film whose source row vanished or was re-keyed
  // (its filmId changed) is dropped wholesale. `recordFilmPruned` is the link-break
  // signal; the document deletes are also counted as reprojection writes.
  private def deleteFilm(filmId: String, reason: String): Unit = {
    projection.removeCard(filmId, audit = "reconcile-prune")
    metrics.recordFilmPruned(reason, 1)
  }
}
