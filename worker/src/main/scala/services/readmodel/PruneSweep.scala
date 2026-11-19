package services.readmodel

import models.{FilmId, StoredMovieRecord}

import play.api.Logging

/**
 * One whole-corpus pass of a [[ReadModelSweeper]] — the FULL reconcile (`reproject = true`) or the
 * cheap PRUNE sweep: the source scan, the heal of a ready row missing a card or a venue, the prune of
 * what no live row projects to ([[OrphanPrune]]), the rolling content check of one slice
 * ([[ReadModelSweeper.ContentSlices]]), the catch-up of what a silent change stream failed to deliver
 * ([[SweepCatchUp]]), and the sweep's summary line. The sweeper schedules it and judges its heals.
 *
 * @param pruneSeconds the prune's cadence: the content slices are numbered by the clock over it.
 * @param logger the sweeper's: a sweep's lines read as the sweeper's, which they are.
 */
private[readmodel] final class PruneSweep(
  projection:     ReadModelSweeper.Projection,
  derivationPass: Option[ReadModelDerivationPass],
  pruneSeconds:   Long,
  override protected val logger: play.api.Logger
) extends ContinuingSteps with Logging {
  import ReadModelProjectionMetrics.{ProjectTrigger, ReconcileKind}

  // The destructive half of a complete sweep: the orphans and unlisted venues it deletes.
  private val orphanPrune = new OrphanPrune(projection, logger)
  // The re-projection of what the `movies` cursor did not deliver, after each prune sweep.
  private val catchUp = new SweepCatchUp(projection, logger)

  // Numbered by the clock, not from zero: a counter each process starts afresh checks
  // slice 0 again after every deploy, and on a day of hourly deploys the rest of the corpus
  // was never reached. Counting on from the clock keeps the next process on the next slice.
  private var sweepCount = projection.clock.instant().getEpochSecond / pruneSeconds

  /** One reconciliation pass over the whole corpus.
   *
   *  `reproject = true` — the FULL sweep: re-project every ready row (the diff keeps
   *  writes minimal — only genuinely changed documents are written) AND prune. This
   *  is the expensive path (projecting ~1400 rows is a ~1-core burst that, on the old
   *  30-min cadence, filled the 320m heap → GC thrash → CPU-credit starvation). It is
   *  NO LONGER SCHEDULED — the resume-token change stream now catches the upserts it
   *  used to (its only unique job); it runs only via the explicit `reconcile()`
   *  seed/backfill primitive (test/fixture seeding).
   *
   *  `reproject = false` — the CHEAP sweep (`pruneOrphans`): build the live-id set from
   *  `ReadModelProjection.filmIds` (no projection) and prune only. This is the frequent
   *  backstop for DELETES / re-keys the change stream's `onMovieDelete` did not retire
   *  (missed while down, or cards a previous process wrote that `lastCardsByRow` never
   *  saw); the set-difference prune is the correct, cheap way to reconcile them.
   *
   *  Self-healing: the prune diffs the ACTUAL read-model ids
   *  (`reader.findAllMovieIds`/`findAllScreeningRefs`) against the live source, NOT this
   *  process's in-memory `lastMovie` — a film a PRIOR worker process wrote and re-keyed
   *  is invisible to a memory-based prune, which is how a re-key across a restart leaked a
   *  duplicate card permanently. Streaming `foreachRecord` (not a whole-corpus `findAll`)
   *  and id-only read-model reads keep the whole corpus off the heap.
   *
   *  A row that fails to project must not abort the prune (the prune is what removes the
   *  duplicates), so each projection is guarded individually. Returns the rows it healed, whether
   *  it completed (scan, projections and prune), and the unlisted venue rows it pruned. */
  def sweep(reproject: Boolean): (Seq[String], Boolean, Seq[ScreeningRef]) = projection.exclusively {
    // Log-only label. The reconcile-sweep metric now tracks ONLY the prune (the live,
    // scheduled backstop); the reproject path survives as a test/backfill seed and is
    // no longer metered — the reproject retirement gate it fed has been removed.
    val kind     = if (reproject) "reproject" else ReconcileKind.Prune
    val baseline = readSweepBaseline(reproject, kind)
    val tally    = new SweepTally
    // The REPROJECT needs showtimes — it writes them. The PRUNE never looks at one: it
    // computes ids, and `filmIds` derives those from the cinema SLOTS, which the slots-only
    // scan still stitches. So the frequent, scheduled sweep no longer pulls the whole
    // `screenings` collection through WiredTiger twice an hour to throw it away — 177,676
    // rows and 129 MB across the five countries (2026-09-05) against a 1.07 GB cache.
    val scan: (StoredMovieRecord => Unit) => tools.ScanOutcome =
      if (reproject) projection.movieRepository.foreachRecord else projection.movieRepository.foreachRecordWithSlots
    val scanComplete = scan(row => sweepRow(row, reproject, kind, baseline, tally)).isComplete
    if (!scanComplete) {
      // The source scan failed mid-way (a Mongo batch read threw after its retries —
      // typically a server-selection / socket timeout while the worker is CPU-throttled,
      // the 2026-06-29 01:34–02:19 served-films flap). `liveIds` is therefore a TRUNCATED
      // view, so pruning on it would delete every read-model card whose source row this
      // scan never reached. Keep the (idempotent) projections we did make and SKIP the
      // destructive prune; the next tick reconciles cleanly once reads recover.
      logger.warn(s"read-model $kind: source scan was incomplete (Mongo read failed mid-scan) — " +
        "skipping the prune this tick to avoid deleting live read-model rows; will retry next tick.")
    } else orphanPrune.prune(reproject, kind, baseline, tally)
    if (!reproject && scanComplete) checkContentSlice(kind, tally.liveRowIds)
    sweepCount += 1
    if (!reproject) catchUp.catchUpUndelivered(kind)
    reportSweep(reproject, kind, tally, scanComplete)
    (tally.healed.toSeq, scanComplete && !tally.projectionFailed && !tally.pruneFailed, tally.prunedUnlisted.toSeq)
  }

  /** Caller holds the projection lock. The read model's cards and screenings rows as they are BEFORE a sweep.
   *
   *  A ready row ANY of whose ids has no card is healed in the same pass, before anything is
   *  pruned: on 2026-09-07 the card id scheme changed under a live read model, the prune removed
   *  509 Polish cards as orphans, and the scrapes took an hour to write the new ones — the
   *  ReadModelFilmPruneBurst alert. A prune must never leave a live film without a card, whatever
   *  id its old card carried. "Any", not "none": most of those 509 were VARIANT cards (a "35 lat
   *  po premierze …" banner, a "przedpremiera" listing) whose plain sibling survived, and a heal
   *  that asked only for rows with no card at all left Warszawa 45 films short four hours later.
   *  …and only when that read was COMPLETE: an incomplete keyset scan answers "no cards", and
   *  healing on that answer would re-project every row (the boot burst this design avoids). A
   *  prune that cannot see the cards heals nothing this tick.
   *
   *  The screenings rows are read once for the venue heal and the prune alike. A venue the source
   *  lists (a cinema slot) with no row here is healed like a missing card: a slot written after the
   *  row's last projection stayed invisible while nothing else touched the row. A read that fails —
   *  or stops short, which the keyset scan reports as EMPTY — heals no venue this tick: "no rows" is
   *  not "could not read them". */
  private def readSweepBaseline(reproject: Boolean, kind: String): SweepBaseline = {
    val cards = if (reproject) tools.ReadOutcome.Answered(Seq.empty[String]) else projection.reader.findAllMovieIdsChecked()
    if (!reproject && cards.answered.isEmpty) logger.warn(s"read-model $kind: the card ids could not be read — no row is healed this tick.")
    SweepBaseline(cards, if (reproject) None else projection.reader.findAllScreeningRefsChecked().answered)
  }

  /** Caller holds the projection lock. One row of a sweep's scan: note its live ids and the venues it no longer
   *  lists, then re-project it (a reproject sweep) or heal what it is missing (a prune sweep). */
  private def sweepRow(row: StoredMovieRecord, reproject: Boolean, kind: String, baseline: SweepBaseline, tally: SweepTally): Unit =
    if (row.readyToProject) {
      // Card ids, venue ids and the projection all come off ONE partition of the row.
      val partition = projection.partition(row)
      val ids = partition.filmIds
      tally.liveIds ++= ids
      tally.liveRowKeys += row.id.value
      tally.liveRowIds  += row.id
      projection.noteRowCards(row.id.value, ids.toSet)
      // The slots-only view lists every venue the row has a slot at, spent or not, so a venue
      // outside it is one the row has left — never a phantom.
      baseline.screeningsByCard.foreach { byCard =>
        lazy val listed = partition.screeningIds.toSet
        ids.foreach(card => byCard.getOrElse(card, Nil).foreach(ref => if (!listed(ref._id)) tally.unlisted += ref))
      }
      if (reproject)
        continuing(s"read-model $kind: a row failed to project")(tally.reprojected += projection.project(partition, ProjectTrigger.Reproject))
          .getOrElse { tally.projectionFailed = true }
      else if (baseline.cardsRead) healIfMissing(row, partition, ids, kind, baseline, tally)
    }

  /** Caller holds the projection lock. Heal a row one of whose cards or venues the baseline lacks. */
  private def healIfMissing(row: StoredMovieRecord, partition: ReadModelProjection.Partition, ids: Seq[String], kind: String,
                            baseline: SweepBaseline, tally: SweepTally): Unit = {
    val metadataHash = ReadModelProjection.metadataHash(row)
    val absentCards  = ids.filterNot(baseline.cardIds)
    val absentVenues =
      baseline.screeningIds.fold(Seq.empty[String])(has => partition.screeningIds.filterNot(has))
        .filterNot(projection.phantomsOf(row.id.value, metadataHash))   // looked already; nothing to write
    if (absentCards.nonEmpty || absentVenues.nonEmpty)
      continuing(s"read-model $kind: a row missing a projection failed to project") {
        projection.heal(row.id, metadataHash, absentCards, absentVenues).foreach { repaired =>
          if (repaired) tally.healed += row.id.value
        }
      }
  }

  /** Caller holds the projection lock. THE ROLLING CONTENT CHECK: one slice of the corpus per sweep, read whole
   *  and re-projected, so a row whose stored projection has drifted is corrected within a day even
   *  though nothing about it changes again. Deterministic by row id, so the slices partition the
   *  corpus rather than sampling it, and every row is reached. */
  private def checkContentSlice(kind: String, liveRowIds: collection.Seq[FilmId]): Unit = {
    val slice = math.floorMod(sweepCount, ReadModelSweeper.ContentSlices.toLong).toInt
    reprojectSlice(liveRowIds, slice, s"$kind sweep", projection.movieRepository.findByIdChecked, projection.projectRow(_, ProjectTrigger.Content))
    derivationPass.foreach(_.armIfUnchecked(sliceWalk(liveRowIds.toVector)))
  }

  /** A sweep's metric and summary line. A `prune` sweep with didWork=true is the deletes/re-keys
   *  the change stream can't deliver, surfaced as kinowo_worker_readmodel_reconcile_sweeps. */
  private def reportSweep(reproject: Boolean, kind: String, tally: SweepTally, scanComplete: Boolean): Unit = {
    val didWork = tally.reprojected > 0 || tally.prunedFilms > 0 || tally.prunedScreenings > 0
    if (!reproject) projection.metrics.recordReconcileSweep(ReconcileKind.Prune, didWork)
    logger.info(s"read-model $kind sweep: reprojected ${tally.reprojected} doc(s), pruned ${tally.prunedFilms} film(s) + " +
      s"${tally.prunedScreenings} orphan screening(s)${if (scanComplete) "" else " [scan INCOMPLETE — prune skipped]"}.")
  }

  /** Caller holds the projection lock. Re-project every row of `rowIds` in content slice `slice`, each `read` by
   *  id and handed to `project`: the documents written, and whether every row was read and
   *  projected (a row gone since `rowIds` was taken counts as done — its cards are the prune's). */
  private def reprojectSlice(rowIds: Iterable[FilmId], slice: Int, what: String,
                             read: FilmId => tools.ReadOutcome[StoredMovieRecord],
                             project: StoredMovieRecord => Int): (Int, Boolean) = {
    var written  = 0
    var complete = true
    rowIds.iterator.filter(ReadModelSweeper.contentSliceOf(_) == slice).foreach { id =>
      val projected = continuing(s"read-model $what: a row in content slice $slice failed to project") {
        read(id) match {
          case tools.ReadOutcome.Answered(row)      => written += project(row)
          case _: tools.ReadOutcome.Absent     => ()
          case _: tools.ReadOutcome.Failed     => complete = false
        }
      }
      if (projected.isEmpty) complete = false
    }
    (written, complete)
  }

  /** The walk a derivation pass makes over `rowIds`, a slice at a time: each row read whole and re-projected, or, for a
   *  cards-only derivation, read slots-only — no `screenings` read — and its cards alone re-projected (`projectCards`). */
  private def sliceWalk(rowIds: Vector[FilmId]): ReadModelSweeper.SliceWalk = new ReadModelSweeper.SliceWalk {
    def rows: Int = rowIds.size
    def reproject(slice: Int, scope: DerivationScope): (Int, Boolean) =
      if (scope == DerivationScope.Cards)
        reprojectSlice(rowIds, slice, "derivation pass (cards)",
                       projection.movieRepository.findByIdWithSlotsChecked, row => projection.projectCards(projection.partition(row)))
      else
        reprojectSlice(rowIds, slice, "derivation pass", projection.movieRepository.findByIdChecked, projection.projectRow(_, ProjectTrigger.Derivation))
  }
}
