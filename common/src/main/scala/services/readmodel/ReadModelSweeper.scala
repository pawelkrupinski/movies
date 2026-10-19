package services.readmodel

import play.api.Logging
import services.movies.{ChangeStreamLiveness, LatestTitleYear, MovieRepository, StoredMovieRecord}
import settings.{ReadModelPruneBootDelay, ReadModelPruneInterval}

import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.util.Try

/**
 * The read model's whole-corpus passes, beside [[ReadModelProjector]]'s per-row projection of each
 * change-stream event: what repairs the read model when an event never arrives.
 *
 *  - THE PRUNE SWEEP (`pruneOrphans`, scheduled): an id-only scan that removes cards and venue rows
 *    no live row projects to, heals a ready row missing a card or a venue, rewrites one rolling
 *    content slice of the corpus, catches up the rows a silent change stream failed to deliver,
 *    and judges which of its heals and rewrites were real misses (`verdictOnSweep`).
 *  - THE FULL RECONCILE (`reconcile`, never scheduled): re-project every row and prune — the
 *    one-shot seed/backfill primitive.
 *  - THE DERIVATION PASS (`advanceDerivationPass`, paced): the whole-corpus re-projection a deploy
 *    owes when it changes what the projection derives from an unchanged row.
 *  - THE BOOT HEAL (`healMissingCards`): every ready row one of whose cards or venues is missing,
 *    projected before the first prune can delete the card it has under an old id.
 *
 * Each projects through the projector it sweeps ([[ReadModelSweeper.Projection]]), under that
 * projector's lock, so a sweep and a stream apply never interleave.
 */
class ReadModelSweeper(
  projection:      ReadModelSweeper.Projection,
  movieRepository: MovieRepository,
  writer:          ReadModelWriter,
  reader:          ReadModelReader,
  metrics:         ReadModelProjectionMetrics,
  clock:           java.time.Clock,
  // How often the id-only orphan prune runs, and how long after boot the first one waits
  // (`KINOWO_READMODEL_PRUNE_SECONDS` / `…_BOOT_DELAY_SECONDS`, resolved by the worker's root).
  pruneInterval:   ReadModelPruneInterval,
  pruneBootDelay:  ReadModelPruneBootDelay,
  // Blocks until the change stream has applied what it had in flight when a sweep let go of the
  // lock — what tells a heal the stream would have made anyway from a real miss (see `verdictOnSweep`).
  awaitStreamApplied: ChangeStreamLiveness => Unit,
  // Which derivation the stored read model was last re-projected whole under — see
  // [[ReadModelDerivationMarker]] and `advanceDerivationPass`. `none` owes no pass.
  derivationMarker:  ReadModelDerivationMarker,
  // Every derivation this code knows, the last its own: what a store behind it is owed (see
  // [[ReadModelDerivation.owedSince]]). A parameter so a spec can stage a history.
  derivationHistory: Seq[Derivation]
) extends ContinuingSteps with Logging {
  import ReadModelProjectionMetrics.{HealTrigger, Op, ProjectTrigger, PruneReason, ReconcileKind, Target}
  import ReadModelSweeper.{ContentSlices, DerivationPass, DerivationPassTickSeconds}
  // The latest year a title may name, read off this class's clock at each ask (`LatestTitleYear`).
  private given LatestTitleYear = LatestTitleYear(clock)

  // The cheap id-only orphan prune runs FREQUENTLY (deletes/re-keys the change stream
  // drops must clear within a tick). The expensive full re-projection is no longer
  // scheduled at all (the resume-token change stream made it redundant — see
  // [[ReadModelProjector]]); it survives only as the explicit `reconcile()` seed/backfill primitive.
  private[readmodel] val PruneSeconds = pruneInterval.value.toSeconds
  // Deferred off the boot path — running a full scan synchronously at `start()` stacked a
  // second scan onto the cache hydrate + first scrape on a cold JVM (the boot CPU drain).
  private[readmodel] val PruneBootDelaySeconds = pruneBootDelay.value.toSeconds

  // Numbered by the clock, not from zero: a counter each process starts afresh checks
  // slice 0 again after every deploy, and on a day of hourly deploys the rest of the corpus
  // was never reached. Counting on from the clock keeps the next process on the next slice.
  private var sweepCount    = clock.instant().getEpochSecond / PruneSeconds
  // The rows the change stream applied or deleted since the running prune sweep started, `None`
  // outside one: what tells a heal the stream would have made anyway from a miss (`verdictOnSweep`).
  private var appliedSinceSweep: Option[scala.collection.mutable.Set[String]] = None
  // The rows the running sweep's content check rewrote, held for the same verdict as its heals:
  // only a row the stream does not then apply itself drifted (`verdictOnSweep`).
  private val sweepDrift = scala.collection.mutable.Buffer.empty[RowDrift]
  // Where the whole-corpus re-projection a derivation change owes stands — see `advanceDerivationPass`.
  private var derivationPass: DerivationPass = DerivationPass.Unchecked

  /** Caller holds the projection lock. The change stream applied (or deleted) `rowId`: a heal or a
   *  rewrite of it by the running sweep was in flight, not missed (`verdictOnSweep`). */
  private[readmodel] def noteApplied(rowId: String): Unit = appliedSinceSweep.foreach(_ += rowId)

  /** Caller holds the projection lock. The content check rewrote a row that disagreed with its
   *  source: a drift candidate for the sweep's verdict. */
  private[readmodel] def noteDrift(row: RowDrift): Unit = sweepDrift += row

  /** Schedule the paced passes on `scheduler`: the orphan prune and the derivation pass's slices. */
  private[readmodel] def schedule(scheduler: ScheduledExecutorService): Unit = {
    // Cheap orphan prune: frequent, no per-row re-projection (can't spike CPU). Deferred
    // off the boot path so it doesn't compete with boot hydrate + the first scrape.
    scheduler.scheduleAtFixedRate(
      () => Try(pruneOrphans()).recover { case exception => logger.warn(s"read-model prune tick failed: ${exception.getMessage}") },
      PruneBootDelaySeconds, PruneSeconds, TimeUnit.SECONDS)
    // The derivation pass's slices, paced: a no-op unless the first sweep found one owed.
    scheduler.scheduleAtFixedRate(
      () => Try(advanceDerivationPass()).recover { case exception => logger.warn(s"read-model derivation pass tick failed: ${exception.getMessage}") },
      PruneBootDelaySeconds, DerivationPassTickSeconds, TimeUnit.SECONDS)
    ()
  }

  // Only `reconcile` calls this — a film whose source row vanished or was re-keyed
  // (its filmId changed) is dropped wholesale. `recordFilmPruned` is the link-break
  // signal; the document deletes are also counted as reprojection writes.
  private def deleteFilm(filmId: String, reason: String): Unit = {
    projection.removeCard(filmId, audit = "reconcile-prune")
    metrics.recordFilmPruned(reason, 1)
  }

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
   *  duplicates), so each projection is guarded individually. */
  private def sweep(reproject: Boolean): (Seq[String], Boolean) = projection.exclusively {
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
      if (reproject) movieRepository.foreachRecord else movieRepository.foreachRecordWithSlots
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
    } else prune(reproject, kind, baseline, tally)
    if (!reproject && scanComplete) checkContentSlice(kind, tally.liveRowIds)
    sweepCount += 1
    if (!reproject) catchUpUndelivered(kind)
    reportSweep(reproject, kind, tally, scanComplete)
    (tally.healed.toSeq, scanComplete && !tally.projectionFailed && !tally.pruneFailed)
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
    val cards = if (reproject) tools.ReadOutcome.Answered(Seq.empty[String]) else reader.findAllMovieIdsChecked()
    if (!reproject && cards.answered.isEmpty) logger.warn(s"read-model $kind: the card ids could not be read — no row is healed this tick.")
    SweepBaseline(cards, if (reproject) None else reader.findAllScreeningRefsChecked().answered)
  }

  /** Caller holds the projection lock. One row of a sweep's scan: note its live ids and the venues it no longer
   *  lists, then re-project it (a reproject sweep) or heal what it is missing (a prune sweep). */
  private def sweepRow(row: StoredMovieRecord, reproject: Boolean, kind: String, baseline: SweepBaseline, tally: SweepTally): Unit =
    if (row.record.readyToProject) {
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

  /** Caller holds the projection lock. The destructive half of a sweep whose scan was COMPLETE: prune off id-only
   *  projections — the prune reads ids/filmIds, never payloads. Each delete guarded: a read-model
   *  write THROWS on failure, and one refused delete must not skip every other orphan, the content
   *  slice, the catch-up and the sweep's metrics. */
  private def prune(reproject: Boolean, kind: String, baseline: SweepBaseline, tally: SweepTally): Unit = {
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
    val cardsToJudge = (if (!reproject) baseline.cards else reader.findAllMovieIdsChecked()).answered.getOrElse {
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
      writer.deleteScreening(ref._id)
      metrics.recordWrite(Target.Screening, Op.Delete, 1)
      projection.forgetScreening(ref)
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
   *  sweep prunes none of them and says so loudly; the content check still rewrites each row. */
  private def pruneUnlistedVenues(kind: String, baseline: SweepBaseline, tally: SweepTally): Unit = {
    val unlisted = tally.unlisted
    val cap      = ReadModelSweeper.unlistedPruneCap(baseline.screeningRefs.fold(0)(_.size))
    if (unlisted.sizeIs > cap) {
      metrics.recordUnlistedVenues(unlisted.size, withheld = true)
      logger.error(s"read-model $kind sweep: ${unlisted.size} venue row(s) whose film no longer lists the venue, over the " +
        s"cap of $cap — pruning NONE; a disagreement between the slots-only view and the projection is likelier than " +
        s"that many missed removals: ${ReadModelSweeper.idsForLog(unlisted.map(_._id))}.")
    } else if (unlisted.nonEmpty) {
      unlisted.foreach(pruneScreening(_, kind, tally))
      metrics.recordUnlistedVenues(unlisted.size, withheld = false)
      logger.warn(s"read-model $kind sweep: pruned ${unlisted.size} venue row(s) whose film no longer lists the venue — " +
        s"a removal the change stream did not apply: ${ReadModelSweeper.idsForLog(unlisted.map(_._id))}.")
    }
  }

  /** Caller holds the projection lock. THE ROLLING CONTENT CHECK: one slice of the corpus per sweep, read whole
   *  and re-projected, so a row whose stored projection has drifted is corrected within a day even
   *  though nothing about it changes again. Deterministic by row id, so the slices partition the
   *  corpus rather than sampling it, and every row is reached. */
  private def checkContentSlice(kind: String, liveRowIds: collection.Seq[services.movies.FilmId]): Unit = {
    val slice = math.floorMod(sweepCount, ContentSlices.toLong).toInt
    reprojectSlice(liveRowIds, slice, s"$kind sweep", movieRepository.findByIdChecked, projection.projectRow(_, ProjectTrigger.Content))
    if (derivationPass == DerivationPass.Unchecked) armDerivationPass(liveRowIds.toVector)
  }

  /** A sweep's metric and summary line. A `prune` sweep with didWork=true is the deletes/re-keys
   *  the change stream can't deliver, surfaced as kinowo_worker_readmodel_reconcile_sweeps. */
  private def reportSweep(reproject: Boolean, kind: String, tally: SweepTally, scanComplete: Boolean): Unit = {
    val didWork = tally.reprojected > 0 || tally.prunedFilms > 0 || tally.prunedScreenings > 0
    if (!reproject) metrics.recordReconcileSweep(ReconcileKind.Prune, didWork)
    logger.info(s"read-model $kind sweep: reprojected ${tally.reprojected} doc(s), pruned ${tally.prunedFilms} film(s) + " +
      s"${tally.prunedScreenings} orphan screening(s)${if (scanComplete) "" else " [scan INCOMPLETE — prune skipped]"}.")
  }

  /** Caller holds the projection lock. THE SELF-HEAL FOR A SILENT CHANGE STREAM (prune sweeps only). A terminal
   *  cursor error reopens itself; a cursor that is open and delivering nothing — a server-side stall,
   *  a stale resume position after a migration — reopens nothing, and the sweep healed a MISSING card
   *  or venue but never re-projected a CHANGED row, so the site served stale ratings and showtimes
   *  until someone restarted the worker. Every row written after the `movies` cursor's last
   *  delivered event is what that cursor has failed to deliver; re-project exactly those. Bounded to
   *  the rows that moved: `updatedAt` is indexed, and in steady state — a live cursor delivers a
   *  write within seconds — the read returns nothing. Whole rows (showtimes included) from a full
   *  stitch, so the projection is the one the stream would have made. Independent of whether the
   *  sweep's own scan completed: it is its own bounded read, and a stale card is a stale card
   *  whether or not the prune could run.
   *
   *  A catch-up that re-projected every row it read raises the floor to the instant its read
   *  started, so while the cursor STAYS dead each sweep reads only what was written since the last
   *  one — not the same rows again, which on a cursor subscribed after the corpus was written was
   *  the whole corpus every sweep. A row whose projection threw holds the floor where it was, so the
   *  next sweep reads it again. The alert on the cursor's age (the DELIVERY floor, which a catch-up
   *  never moves) is what ends the state. Only while a movies cursor is SUBSCRIBED: a repository
   *  that never opened one (a test wiring, a Mongo-less boot) has promised no deliveries, and the
   *  prune must stay the id-only sweep it is there. */
  private def catchUpUndelivered(kind: String): Unit = {
    val liveness = movieRepository.changeStreamLiveness
    if (liveness.isWatching(ChangeStreamLiveness.Movies)) {
      val readFrom = liveness.now()
      val floor    = liveness.catchUpFloor(ChangeStreamLiveness.Movies)
      // Whether every event the cursors delivered before this read had been applied when it began.
      // Until then a row inside the overlap may be one a LIVE cursor delivered and whose apply is
      // still queued: projecting it is harmless, but it is no write the cursor missed.
      val backlogApplied = liveness.appliedThrough(liveness.lastTicket)
      var caughtUp = 0
      var failed   = false
      val past     = scala.collection.mutable.Set.empty[String]
      def catchUp(row: StoredMovieRecord)(counted: Int => Boolean): Unit =
        if (row.record.readyToProject)
          continuing(s"read-model $kind: a row written since the change stream last delivered failed to project") {
            if (counted(projection.projectRow(row, ProjectTrigger.CatchUp))) caughtUp += 1
          }.getOrElse { failed = true }
      // Every row stamped past the floor: what the cursor has not been seen to deliver.
      val complete = movieRepository.foreachRecordUpdatedSince(floor) { row => past += row.id.value; catchUp(row)(_ => true) }
        // …and the overlap under it: a write stamped under the floor may have committed after the
        // last catch-up's scan (see `MovieRepository.CatchUpOverlap`). A row there that the stream
        // or the last catch-up already projected writes nothing again; only one whose projection
        // was stale — the write that landed behind the scan — counts as caught up, and only while
        // no delivered event still waits for its apply (see `backlogApplied`).
        .andThen(movieRepository.foreachRecordUpdatedSince(floor.minus(MovieRepository.CatchUpOverlap)) { row =>
          if (!past(row.id.value)) catchUp(row)(written => written > 0 && backlogApplied)
        })
      if (complete.isComplete && !failed) liveness.caughtUp(ChangeStreamLiveness.Movies, readFrom)
      metrics.recordCatchUp(caughtUp)
      if (caughtUp > 0)
        logger.warn(s"read-model $kind sweep: re-projected $caughtUp row(s) written since the movies change stream last " +
          s"delivered ($floor) — the cursor is open but not delivering them.")
    }
  }

  /** Which of the rows a prune sweep healed, or its content check rewrote, were MISSES — the rest
   *  the change stream had in flight.
   *
   *  The sweep holds the projection lock from its first read of the read model to its last
   *  write, and every stream apply waits for that lock. So a scrape landing a new venue or a new
   *  row seconds before the sweep, or during it, leaves exactly the id the sweep finds absent in
   *  an event queued behind it: the heal writes it, and the stream would have written it a moment
   *  later. Counted, those paged `ReadModelHealsRecurring` for a stream that was fine — every
   *  sweep heal from 2026-09-24 21:08Z to 09-25 02:37Z (US and UK) came with a scrape finishing
   *  within five seconds of the sweep starting, and with no retirement, failed re-read or restart
   *  behind it. So the verdict waits, outside the lock, for the stream to apply what it had in
   *  flight: a healed row the stream then applies was not missed; one it leaves alone was. Only
   *  misses are metered and named in the WARN line.
   *
   *  The content check's rewrites race the stream the same way, and more often: the stream holds
   *  a film's re-read for up to `Debounce.Worker.cap`, so a showtime change landing in the two
   *  minutes before the check reaches its row is projected by the check first. Every such row
   *  counted as drift — petsematary|1989 at Regal Kendall Village Miami on 2026-10-07, its source
   *  slot written 38 s before the check rewrote it, the stream applying it once the sweep let go. */
  private def verdictOnSweep(healed: Seq[String], rewritten: Seq[RowDrift]): Unit = {
    if (healed.nonEmpty || rewritten.nonEmpty) awaitStreamApplied(movieRepository.changeStreamLiveness)
    val applied = projection.exclusively { val rows = appliedSinceSweep.fold(Set.empty[String])(_.toSet); appliedSinceSweep = None; rows }
    val (raced, drifted) = rewritten.partition(row => applied(row.rowId))
    if (raced.nonEmpty)
      logger.info(s"read-model content check: rewrote ${raced.size} row(s) the change stream had in flight and then " +
        s"applied itself — not drift: ${ReadModelSweeper.idsForLog(raced.map(_.rowId))}.")
    drifted.foreach { row =>
      row.causes.foreach { case (cause, documents) => metrics.recordDrift(cause, documents) }
      logger.warn(s"read-model content check: row ${row.rowId} had drifted from its source — ${row.detail}.")
    }
    val (inFlight, missed) = healed.partition(applied)
    if (inFlight.nonEmpty)
      logger.info(s"read-model prune sweep: wrote ${inFlight.size} row(s) the change stream had in flight and then " +
        s"applied itself — not heals: ${ReadModelSweeper.idsForLog(inFlight)}.")
    if (missed.nonEmpty) {
      metrics.recordHeal(HealTrigger.Sweep, missed.size)
      logger.warn(s"read-model prune sweep: projected ${missed.size} ready row(s) missing a card or a venue " +
        s"before the prune: ${ReadModelSweeper.idsForLog(missed)}.")
    }
  }

  /** Full re-projection + prune. NOT scheduled — the resume-token change stream made the
   *  periodic reproject redundant. Kept as an explicit one-shot seed/backfill primitive:
   *  fixture/e2e read-model seeding calls it to project a settled corpus synchronously
   *  (it stitches split films via `foreachRecord`, which a per-row `onMovieUpsert` seed
   *  would not). Mirrors `scripts.BackfillReadModel`.
   *
   *  Returns whether the source scan was COMPLETE, every row it read projected and every orphan it
   *  found was pruned. An incomplete one (a page whose read failed) projected only the rows it could
   *  read whole, a row whose projection threw may stand half-written, and a prune that threw leaves
   *  an orphan served, so a caller seeding a read model it then asserts on owes another reconcile
   *  once Mongo recovers. */
  def reconcile(): Boolean = sweep(reproject = true)._2

  /** Cheap id-only orphan prune — the frequent backstop for deleted / merged-away rows, and
   *  for the rows a silent change stream failed to deliver (see `sweep`). */
  def pruneOrphans(): Unit = {
    projection.exclusively { appliedSinceSweep = Some(scala.collection.mutable.Set.empty); sweepDrift.clear() }
    val (healed, rewritten) =
      try { val healed = sweep(reproject = false)._1; projection.exclusively((healed, sweepDrift.toSeq)) }
      catch { case exception: Throwable => projection.exclusively { appliedSinceSweep = None }; throw exception }
    verdictOnSweep(healed, rewritten)
  }

  /** Caller holds the projection lock. Re-project every row of `rowIds` in content slice `slice`, each `read` by
   *  id and handed to `project`: the documents written, and whether every row was read and
   *  projected (a row gone since `rowIds` was taken counts as done — its cards are the prune's). */
  private def reprojectSlice(rowIds: Iterable[services.movies.FilmId], slice: Int, what: String,
                             read: services.movies.FilmId => tools.ReadOutcome[StoredMovieRecord],
                             project: StoredMovieRecord => Int): (Int, Boolean) = {
    var written  = 0
    var complete = true
    rowIds.iterator.filter(ReadModelSweeper.contentSliceOf(_) == slice).foreach { id =>
      val projected = continuing(s"read-model $what: a row in content slice $slice failed to project") {
        read(id) match {
          case tools.ReadOutcome.Answered(row)      => written += project(row)
          case tools.ReadOutcome.Absent(_) => ()
          case tools.ReadOutcome.Failed(_) => complete = false
        }
      }
      if (projected.isEmpty) complete = false
    }
    (written, complete)
  }

  /** Caller holds the projection lock. Read which derivation the stored read model was last re-projected whole
   *  under and, when this code derives another, start the pass it owes over `rowIds` — the cards
   *  alone when every derivation since moved only cards ([[ReadModelDerivation.owedSince]]), and
   *  from the slice a previous process's pass towards the same version had reached. A read that
   *  fails decides nothing: the next sweep reads again. */
  private def armDerivationPass(rowIds: Vector[services.movies.FilmId]): Unit =
    (for (recorded <- derivationMarker.recorded(); progress <- derivationMarker.progress()) yield (recorded, progress)) match {
      case scala.util.Failure(exception) =>
        logger.warn(s"read-model derivation pass: the recorded derivation could not be read (${exception.getMessage}) — " +
          "asking again next sweep.")
      case scala.util.Success((recorded, progress)) =>
        ReadModelDerivation.owedSince(recorded, derivationHistory) match {
          case None =>
            derivationPass = DerivationPass.Current
            resetStaleProgress(progress)
          case Some(scope) => startDerivationPass(rowIds, recorded, progress, scope)
        }
    }

  /** Progress towards some other version is void once this store is current under this code: a pass
   *  for version B stopped at slice k, then a rollback to A (current), and A's change-stream
   *  projections rewrite cards in slices before k under A. Resumed at k on the roll forward, B's pass
   *  would never reach them — so the progress starts over. */
  private def resetStaleProgress(progress: Option[DerivationProgress]): Unit = {
    val reset = DerivationProgress(derivationHistory.last.version, 0)
    if (progress.exists(_ != reset))
      continuing("read-model derivation pass: resetting a stale pass's progress failed; a later pass may resume past rows it owes") {
        derivationMarker.recordProgress(reset)
      }
  }

  /** Caller holds the projection lock. Start the pass `scope` owes over `rowIds`, from the slice a previous
   *  process's pass towards the same version had reached. */
  private def startDerivationPass(rowIds: Vector[services.movies.FilmId], recorded: Option[DerivationVersion],
                                  progress: Option[DerivationProgress], scope: DerivationScope): Unit = {
    val current = derivationHistory.last.version
    val from    = progress.collect { case DerivationProgress(`current`, next) => next }.getOrElse(0)
    derivationPass = DerivationPass.Running(rowIds, scope, nextSlice = from, written = 0, complete = true)
    logger.warn(s"read-model derivation pass: the stored read model was derived under " +
      s"${recorded.getOrElse("no recorded version")}, this worker derives $current — re-projecting " +
      s"${if (scope == DerivationScope.Cards) "the cards of" else "all"} ${rowIds.size} row(s), one content slice of " +
      s"$ContentSlices every ${DerivationPassTickSeconds}s" +
      (if (from > 0) s", resuming at slice $from where an earlier pass stopped." else "."))
  }

  /** THE DERIVATION PASS: one content slice of the whole-corpus re-projection a derivation change
   *  owes, per tick.
   *
   *  A deploy that changes what the projection DERIVES from an unchanged row moves no row, so the
   *  change stream re-projects nothing and each stored card keeps the old derivation until the
   *  rolling content check reaches its slice — up to a day. On 2026-09-24 the poster selection
   *  changed that way and `ReadModelContentMismatch` held ~2.5 h, until a human forced a
   *  whole-corpus content check by hand. So the first complete prune sweep compares this code's
   *  derivation ([[ReadModelDerivation.current]]) with the one the store recorded and, when they
   *  differ, walks the same 48 content slices over every row it saw — one slice per tick, each
   *  under the lock only for its own rows, so the change stream keeps flowing and Mongo takes a
   *  slice's reads at a time: the whole corpus in 8 minutes, inside the content audit's
   *  15-minute re-check. Diff-based like every projection, so only cards that moved are written.
   *  When only card fields moved it reads each row slots-only and re-projects its cards alone.
   *  Each complete slice records the pass's progress, so a restart resumes rather than repeats.
   *  Only a pass that read and projected every row records the new version; one that did not
   *  leaves the old one, and the next sweep starts it again from the first slice that missed a
   *  row. Nothing happens when the store's version is this process's. */
  def advanceDerivationPass(): Unit = projection.exclusively {
    derivationPass match {
      case running: DerivationPass.Running =>
        val (written, complete) = reprojectDerivationSlice(running)
        val next = running.copy(nextSlice = running.nextSlice + 1, written = running.written + written,
                                complete = running.complete && complete)
        // Only while every slice so far was complete: a restart then resumes after the last one,
        // and a pass that missed a row starts over from the first slice it missed one in.
        if (next.complete && next.nextSlice < ContentSlices)
          continuing("read-model derivation pass: recording its progress failed; a restart repeats this slice") {
            derivationMarker.recordProgress(DerivationProgress(derivationHistory.last.version, next.nextSlice))
          }
        derivationPass = if (next.nextSlice < ContentSlices) next else finishDerivationPass(next)
      case _ => ()
    }
  }

  /** Caller holds the projection lock. One slice of a running pass. A cards-only derivation reads each row
   *  slots-only — no `screenings` read — and re-projects its cards alone (`projectCards`). */
  private def reprojectDerivationSlice(running: DerivationPass.Running): (Int, Boolean) =
    if (running.scope == DerivationScope.Cards)
      reprojectSlice(running.rows, running.nextSlice, "derivation pass (cards)",
                     movieRepository.findByIdWithSlotsChecked, row => projection.projectCards(projection.partition(row)))
    else
      reprojectSlice(running.rows, running.nextSlice, "derivation pass",
                     movieRepository.findByIdChecked, projection.projectRow(_, ProjectTrigger.Derivation))

  /** Caller holds the projection lock. A pass past its last slice: recorded when it read and projected every
   *  row, otherwise left unrecorded for the next sweep to run again. */
  private def finishDerivationPass(done: DerivationPass.Running): DerivationPass =
    if (!done.complete) {
      logger.warn(s"read-model derivation pass: re-projected ${done.rows.size} row(s), rewrote ${done.written} " +
        "document(s), but some rows could not be read or projected — the derivation stays unrecorded and the " +
        "next sweep runs the pass again.")
      DerivationPass.Unchecked
    } else
      continuing("read-model derivation pass: recording the derivation failed; the next sweep runs the pass again") {
        derivationMarker.record(derivationHistory.last.version)
        logger.info(s"read-model derivation pass: re-projected ${if (done.scope == DerivationScope.Cards) "the cards of " else ""}" +
          s"all ${done.rows.size} row(s) under ${derivationHistory.last.version}, rewriting ${done.written} document(s); recorded.")
        DerivationPass.Current
      }.getOrElse(DerivationPass.Unchecked)

  /** Project, at boot, every ready row one of whose cards is missing — before the
   *  first prune can delete the card it has under an old id.
   *
   *  A row's card id can change under a running read model: the id scheme moved
   *  (2026-09, from `sanitize(title)|resolvedYear` to the row's permanent `FilmId`), or
   *  a database was restored from before that. The scheduled prune then deletes the
   *  old-id cards as orphans, while the change stream re-creates only the rows a
   *  scrape happens to rewrite — so the served site runs short until every venue has
   *  been scraped again. The first version of this check gated a whole-corpus
   *  reproject on a MAJORITY of cards being stale, and the 2026-09-07 rollout came in
   *  under it (509 of 1,156 Polish cards): Poznań served ~10% fewer films for the
   *  better part of an hour. The exact question is cheaper and has no threshold:
   *  which ready rows lack a card under one of their ids? Those are projected whole;
   *  everything else is left to the diffing change-stream path. A row is asked for
   *  EVERY id it projects to — a split row whose plain card survived but whose variant
   *  card did not is still short, and the change stream only revisits it on its next
   *  scrape change. */
  private[readmodel] def healMissingCards(study: Option[Seq[BootRow]]): Unit = Try {
    val cardIds = reader.findAllMovieIdsChecked().answered
    val refs    = reader.findAllScreeningRefsChecked().answered
    val (missing, complete) = findMissingAtBoot(study, cardIds, refs)
    if (cardIds.isEmpty) logger.warn("read model: the card ids could not be read at boot — nothing healed; the prune sweep retries.")
    // Each row guarded, as in the sweep: one failed write must not leave every later row unhealed.
    val projected = missing.flatMap { row =>
      continuing(s"read model: healing ${row.id} at boot failed")(projection.exclusively(projection.heal(row.id, row.metadataHash, row.absentCards, row.absentVenues)))
        .flatten.map(written => (row.id.value, written, row.refile)) }
    val (refiledRows, lostRows) = projected.collect { case (id, true, refile) => (id, refile) }.partition(_._2)
    reportBootHeals(lostRows.map(_._1), refiledRows.map(_._1), complete)
  }.recover { case exception => logger.warn(s"read-model missing-card check failed, skipped: ${exception.getMessage}") }

  /** The ready rows one of whose cards or venues the read model lacks, and whether the source read
   *  was complete. The check reads slots only (ids derive from the slot titles, never from a
   *  showtime); the few rows it names are then read whole, showtimes included. An incomplete card
   *  read heals nothing: "no cards" and "could not read" differ. The boot hydrate's rows when it
   *  handed them over (see [[BootCorpusStudy]]); otherwise a read of its own. */
  private def findMissingAtBoot(study: Option[Seq[BootRow]], cardIds: Option[Seq[String]],
                                refs: Option[Seq[ScreeningRef]]): (Seq[MissingAtBoot], Boolean) = {
    val cards   = cardIds.fold(Set.empty[String])(_.toSet)
    val venues  = refs.map(_.map(_._id).toSet)
    // Each (card, venue) the read model serves, whichever city it is filed under: a missing venue row
    // whose card and venue are here under ANOTHER city is a re-file -- a deploy moved the venue's
    // city (2026-10-11: Germany's and the US's town pages, 1,029 and 1,508 rows) -- not a lost write.
    val filed   = refs.fold(Set.empty[(String, String)])(_.iterator.flatMap(_.venueKey).toSet)
    val missing = scala.collection.mutable.ListBuffer.empty[MissingAtBoot]
    def refiled(filmIds: Seq[String], venue: String): Boolean =
      filmIds.filter(card => venue.startsWith(card + "|")).maxByOption(_.length)
        .flatMap(card => ScreeningRef(venue, card).venueKey).exists(filed)
    def check(id: services.movies.FilmId, filmIds: Seq[String], screeningIds: Seq[String], metadataHash: => Int): Unit = {
      projection.recordRowCards(id.value, filmIds.toSet)
      val absentCards  = filmIds.filterNot(cards)
      val absentVenues = venues.fold(Seq.empty[String])(has => screeningIds.filterNot(has))
      if (absentCards.nonEmpty || absentVenues.nonEmpty)
        missing += MissingAtBoot(id, metadataHash, absentCards, absentVenues, absentCards.isEmpty && absentVenues.forall(refiled(filmIds, _)))
    }
    val complete = cardIds.isDefined && study.fold(movieRepository.foreachRecordWithSlots { row =>
      if (row.record.readyToProject) {
        val partition = projection.partition(row)
        check(row.id, partition.filmIds, partition.screeningIds, ReadModelProjection.metadataHash(row))
      }
    }.isComplete) { rows => rows.foreach(row => check(row.id, row.filmIds, row.screeningIds, row.metadataHash)); true }
    (missing.toSeq, complete)
  }

  /** The boot heal's metrics and lines: rows healed of a lost write, and rows whose venues moved city. */
  private def reportBootHeals(healed: Seq[String], refiled: Seq[String], complete: Boolean): Unit = {
    if (healed.nonEmpty) {
      metrics.recordHeal(HealTrigger.Boot, healed.size)
      logger.warn(s"read model: projected ${healed.size} ready row(s) missing a card or a venue at boot" +
        (if (complete) "" else " (source scan incomplete — the rest heal on their next change)") +
        s": ${ReadModelSweeper.idsForLog(healed)}.")
    }
    if (refiled.nonEmpty) {
      metrics.recordHeal(HealTrigger.Refile, refiled.size)
      logger.info(s"read model: projected ${refiled.size} ready row(s) at boot whose venues the read model serves under " +
        s"another city — this code files them elsewhere: ${ReadModelSweeper.idsForLog(refiled)}.")
    }
  }
}

object ReadModelSweeper {
  /** What a sweep asks of the projector it sweeps: its lock, its per-row projection, and the memo
   *  of what it last wrote — which a sweep reads and corrects, since the read model, not the memo,
   *  is the truth. Every method but `exclusively` and `partition` expects the lock held. */
  trait Projection {
    /** Run `body` under the projection lock: every change-stream apply waits for it. */
    def exclusively[A](body: => A): A
    /** The row split into its cards' display-title groups, under the repository's title rules. */
    def partition(row: StoredMovieRecord): ReadModelProjection.Partition
    /** Project a ready row's partition and write what changed: the documents written. */
    def project(partition: ReadModelProjection.Partition, trigger: ReadModelProjectionMetrics.ProjectTrigger): Int
    /** [[project]] a row read whole. */
    def projectRow(row: StoredMovieRecord, trigger: ReadModelProjectionMetrics.ProjectTrigger): Int
    /** Re-project only the cards of a row read without its showtimes: the cards rewritten. */
    def projectCards(partition: ReadModelProjection.Partition): Int
    /** Re-project a row some of whose cards or venues are missing: whether it repaired one, `None`
     *  when nothing was projected. */
    def heal(id: services.movies.FilmId, metadataHash: Int, absentCards: Seq[String], absentVenues: Seq[String]): Option[Boolean]
    /** The venues a heal found to be phantoms of this row, while its metadata is what it was. */
    def phantomsOf(rowId: String, metadataHash: Int): Set[String]
    /** The card ids a row projects to, remembered when the projector has none for it yet. */
    def noteRowCards(rowId: String, cards: Set[String]): Unit
    /** The card ids a row projects to, remembered over whatever the projector had. */
    def recordRowCards(rowId: String, cards: Set[String]): Unit
    /** Forget every source row but the live ones — after a COMPLETE scan only. */
    def retainRows(live: String => Boolean): Unit
    /** Remove a card and its remembered screenings from the read model; `audit` names the path. */
    def removeCard(filmId: String, audit: String): Unit
    /** Forget a screenings row a sweep deleted. */
    def forgetScreening(ref: ScreeningRef): Unit
  }

  /** How many prune sweeps it takes to re-project the whole corpus once — the ROLLING
   *  CONTENT CHECK. Every backstop before it compared IDS: the prune removes a card whose
   *  row is gone, the heal writes a card or a venue that is missing. None of them can see a
   *  row that EXISTS and is WRONG, and on 2026-09-08 three UK films had held the wrong
   *  showtimes since 2026-08-29 — Troy and 2046 at the Prince Charles, Glastonbury at the
   *  Southsea — served to real users, invisible to every sweep, and unrepairable by the
   *  change stream because the source had long since stopped changing. A projection is
   *  diff-based, so re-projecting a slice costs a read per row and writes only what actually
   *  drifted; at 48 slices on a 30-minute sweep the whole corpus is verified once a day.
   *
   *  A constant, not a knob: its one override was 1, to push a derivation change out at once
   *  (2026-09-24), and the derivation pass now does that on its own. */
  private[readmodel] val ContentSlices = 48

  /** The content slice a row falls in: by its id, so the slices partition the corpus. */
  private[readmodel] def contentSliceOf(id: services.movies.FilmId): Int =
    math.floorMod(id.value.##.toLong, ContentSlices.toLong).toInt

  /** Seconds between two slices of a derivation pass: 48 slices, the whole corpus in 8 minutes. */
  private val DerivationPassTickSeconds = 10L

  /** Where the whole-corpus re-projection a derivation change owes stands. */
  private sealed trait DerivationPass
  private object DerivationPass {
    /** The store's recorded derivation has not been read (yet, or again after a failed pass). */
    case object Unchecked extends DerivationPass
    /** The store was derived under this process's version: nothing owed. */
    case object Current extends DerivationPass
    /** Re-projecting `rows` (their cards alone, for a `Cards` scope), slice `nextSlice` next;
     *  `complete` while every row so far was read and projected. */
    final case class Running(rows: Vector[services.movies.FilmId], scope: DerivationScope, nextSlice: Int, written: Int,
                             complete: Boolean) extends DerivationPass
  }

  /** The id-only orphan prune's compiled-in cadence: every 30 minutes, first 5 minutes after boot. */
  val DefaultPruneInterval: ReadModelPruneInterval =
    ReadModelPruneInterval(scala.concurrent.duration.Duration(1800L, TimeUnit.SECONDS))
  val DefaultPruneBootDelay: ReadModelPruneBootDelay =
    ReadModelPruneBootDelay(scala.concurrent.duration.Duration(300L, TimeUnit.SECONDS))

  /** How long a write that landed while a sweep held the lock may take to be DELIVERED by its
   *  cursor, before the wait below can see it queued. */
  private val InFlightDeliveryGrace = scala.concurrent.duration.Duration(2000L, TimeUnit.MILLISECONDS)
  /** The longest a sweep's heal verdict waits for the stream's apply thread to catch up: past the
   *  longest a debounce holds a film's re-read (its cap), plus the re-read itself. At 30 s, under a
   *  2-minute cap, a film the debounce was holding was counted a miss whenever its burst outlasted
   *  the wait — `ReadModelHealsRecurring` in the US, UK and Germany from 2026-09-30. */
  private[services] val InFlightApplyTimeout: scala.concurrent.duration.FiniteDuration =
    services.movies.MovieChangeStream.Debounce.Worker.cap + scala.concurrent.duration.Duration(60L, TimeUnit.SECONDS)

  /** Let the change stream apply what it had in flight when a sweep let go of the lock: wait out
   *  the delivery of a write that landed during the sweep, then until every event handed to the
   *  apply thread by then has been applied, or the timeout — an apply thread that far behind is
   *  `ChangeStreamApplyLagging`'s business, and a heal it leaves unapplied counts. No cursor
   *  subscribed (a Mongo-less boot, a test wiring) means nothing is in flight. */
  def awaitStreamApplied(
    liveness: ChangeStreamLiveness,
    grace:    scala.concurrent.duration.FiniteDuration = InFlightDeliveryGrace,
    timeout:  scala.concurrent.duration.FiniteDuration = InFlightApplyTimeout
  ): Unit =
    if (ChangeStreamLiveness.Collections.exists(liveness.isWatching)) {
      Thread.sleep(grace.toMillis)
      val handedOff = liveness.lastTicket
      val deadline  = System.nanoTime() + timeout.toNanos
      while (!liveness.appliedThrough(handedOff) && System.nanoTime() < deadline) Thread.sleep(50)
    }

  /** The most unlisted venue rows one sweep prunes: 20, or 1% of the read model's venue rows when
   *  that is more. A stream's missed removals come a few at a time (one on 2026-09-30); past this,
   *  the comparison is the likelier fault, and the sweep prunes none. */
  private[readmodel] def unlistedPruneCap(screeningRows: Int): Int = math.max(20, screeningRows / 100)

  /** How many row ids one heal line names before it summarises the rest. */
  private[readmodel] val LoggedIdsPerLine = 20

  /** The healed row ids for a log line — every heal is otherwise a count nobody can trace
   *  back to the retirement that caused it — capped so a mass heal stays one line. */
  private[readmodel] def idsForLog(ids: collection.Seq[String]): String =
    ids.take(LoggedIdsPerLine).mkString(", ") +
      (if (ids.sizeIs > LoggedIdsPerLine) s" (+${ids.size - LoggedIdsPerLine} more)" else "")
}
