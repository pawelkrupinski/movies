package services.readmodel

import tools.{ScheduledTick, TickRecorder, TickRun}

import play.api.Logging
import services.movies.{ChangeStreamLiveness, MovieChangeFeed, MovieRepository, MovieRepositoryReader, StoredMovieRecord}
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
 *    and judges which of its heals and rewrites were real misses ([[SweepVerdict]]).
 *  - THE FULL RECONCILE (`reconcile`, never scheduled): re-project every row and prune — the
 *    one-shot seed/backfill primitive.
 *  - THE DERIVATION PASS ([[ReadModelDerivationPass]], paced): the whole-corpus re-projection a
 *    deploy owes when it changes what the projection derives from an unchanged row; the first
 *    complete prune sweep arms it with a walk over the rows it saw ([[ReadModelSweeper.SliceWalk]]).
 *  - THE BOOT HEAL (`healMissingCards`): every ready row one of whose cards or venues is missing,
 *    projected before the first prune can delete the card it has under an old id.
 *
 * Each projects through the projector it sweeps ([[ReadModelSweeper.Projection]]), under that
 * projector's lock, so a sweep and a stream apply never interleave. Built over a projector's
 * `projection` by the composition root (`ReadModelWiring`), which also schedules its paced passes
 * ([[schedule]]); the projector runs its boot heal ([[healMissingCards]]) between its boot reads.
 */
class ReadModelSweeper(
  projection:      ReadModelSweeper.Projection,
  // How often the id-only orphan prune runs, and how long after boot the first one waits
  // (`KINOWO_READMODEL_PRUNE_SECONDS` / `…_BOOT_DELAY_SECONDS`, resolved by the worker's root).
  pruneInterval:   ReadModelPruneInterval  = ReadModelSweeper.DefaultPruneInterval,
  pruneBootDelay:  ReadModelPruneBootDelay = ReadModelSweeper.DefaultPruneBootDelay,
  // Blocks until the change stream has applied what it had in flight when a sweep let go of the
  // lock — what tells a heal the stream would have made anyway from a real miss (see [[SweepVerdict]]).
  // The root passes the wait on its own `sleep`; the default serves the specs that build a sweeper bare.
  awaitStreamApplied: ChangeStreamLiveness => Unit = ReadModelSweeper.awaitStreamApplied(_, tools.Sleep.Blocking),
  // The derivation pass the first complete sweep arms, and `schedule` paces; `None` owes no pass.
  derivationPass: Option[ReadModelDerivationPass] = None
) extends ContinuingSteps with Logging {
  import ReadModelProjectionMetrics.{HealTrigger, ProjectTrigger, ReconcileKind}
  import ReadModelSweeper.{ContentSlices, SliceWalk}
  // The stores and clock the projection writes and keeps time by: a sweep passes over exactly those.
  private val movieRepository = projection.movieRepository
  private val reader          = projection.reader
  private val metrics         = projection.metrics
  private val clock           = projection.clock
  // The destructive half of a complete sweep: the orphans and unlisted venues it deletes.
  private val pruner          = new SweepPrune(projection, logger)
  // What the stream did while a prune sweep held the lock, and so which of its heals and rewrites were misses.
  private val verdict         = new SweepVerdict(projection, () => awaitStreamApplied(movieRepository.changeStreamLiveness), logger)

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
  /** Schedule the paced passes on `scheduler`: the orphan prune and the derivation pass's slices,
   *  each completed tick reported to `ticks` (`ScheduledTickStale`). The worker's root calls it once
   *  the projector's stream watch is up. */
  def schedule(scheduler: ScheduledExecutorService, ticks: TickRecorder): Unit = {
    val prune      = scala.concurrent.duration.Duration(PruneSeconds, TimeUnit.SECONDS)
    val bootDelay  = scala.concurrent.duration.Duration(PruneBootDelaySeconds, TimeUnit.SECONDS)
    ticks.started("read-model-prune", prune, bootDelay)
    // Cheap orphan prune: frequent, no per-row re-projection (can't spike CPU). Deferred
    // off the boot path so it doesn't compete with boot hydrate + the first scrape.
    scheduler.scheduleAtFixedRate(
      () => ScheduledTick.recorded("read-model-prune", prune, logger, ticks)(pruneOrphans()),
      PruneBootDelaySeconds, PruneSeconds, TimeUnit.SECONDS)
    // The derivation pass's slices, paced: a no-op unless the first sweep found one owed.
    derivationPass.foreach { pass =>
      val derivation = scala.concurrent.duration.Duration(ReadModelDerivationPass.TickSeconds, TimeUnit.SECONDS)
      ticks.started("read-model-derivation-pass", derivation, bootDelay)
      scheduler.scheduleAtFixedRate(
        () => ScheduledTick.recorded("read-model-derivation-pass", derivation, logger, ticks)(pass.advance()),
        PruneBootDelaySeconds, ReadModelDerivationPass.TickSeconds, TimeUnit.SECONDS)
    }
    logger.info(s"read-model sweeps scheduled: orphan-prune every ${PruneSeconds}s (first in ${PruneBootDelaySeconds}s).")
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
  private def sweep(reproject: Boolean): (Seq[String], Boolean, Seq[ScreeningRef]) = projection.exclusively {
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
    } else pruner.prune(reproject, kind, baseline, tally)
    if (!reproject && scanComplete) checkContentSlice(kind, tally.liveRowIds)
    sweepCount += 1
    if (!reproject) catchUpUndelivered(kind)
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

  /** Caller holds the projection lock. THE ROLLING CONTENT CHECK: one slice of the corpus per sweep, read whole
   *  and re-projected, so a row whose stored projection has drifted is corrected within a day even
   *  though nothing about it changes again. Deterministic by row id, so the slices partition the
   *  corpus rather than sampling it, and every row is reached. */
  private def checkContentSlice(kind: String, liveRowIds: collection.Seq[services.movies.FilmId]): Unit = {
    val slice = math.floorMod(sweepCount, ContentSlices.toLong).toInt
    reprojectSlice(liveRowIds, slice, s"$kind sweep", movieRepository.findByIdChecked, projection.projectRow(_, ProjectTrigger.Content))
    derivationPass.foreach(_.armIfUnchecked(sliceWalk(liveRowIds.toVector)))
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
      val pass = new CatchUpPass(kind, liveness.catchUpFloor(ChangeStreamLiveness.Movies), liveness.appliedThrough(liveness.lastTicket))
      if (pass.run().isComplete && !pass.failed) liveness.caughtUp(ChangeStreamLiveness.Movies, readFrom)
      metrics.recordCatchUp(pass.caughtUp)
      if (pass.caughtUp > 0)
        logger.warn(s"read-model $kind sweep: re-projected ${pass.caughtUp} row(s) written since the movies change stream last " +
          s"delivered (${pass.floor}) — the cursor is open but not delivering them.")
    }
  }

  /** One [[catchUpUndelivered]]'s reads past `floor`. `backlogApplied`: every event the cursors delivered before it
   *  had been applied when it began — else an overlap row may be a LIVE cursor's, its apply still queued, no miss. */
  private final class CatchUpPass(kind: String, val floor: java.time.Instant, backlogApplied: Boolean) {
    var (caughtUp, failed) = (0, false)
    private val past = scala.collection.mutable.Set.empty[String]

    /** Every row stamped past the floor: what the cursor has not been seen to deliver. …and the overlap under it: a
     *  write stamped under the floor may have committed after the last catch-up's scan (`MovieRepository.CatchUpOverlap`).
     *  A row there the stream or the last catch-up already projected writes nothing again; only one whose projection was
     *  stale counts as caught up, and only while no delivered event still waits for its apply (`backlogApplied`). */
    def run(): tools.ScanOutcome =
      movieRepository.foreachRecordUpdatedSince(floor) { row => past += row.id.value; catchUp(row)(_ => true) }
        .andThen(movieRepository.foreachRecordUpdatedSince(floor.minus(MovieRepository.CatchUpOverlap)) { row =>
          if (!past(row.id.value)) catchUp(row)(written => written > 0 && backlogApplied)
        })

    private def catchUp(row: StoredMovieRecord)(counted: Int => Boolean): Unit =
      if (row.record.readyToProject)
        continuing(s"read-model $kind: a row written since the change stream last delivered failed to project") {
          if (counted(projection.projectRow(row, ProjectTrigger.CatchUp))) caughtUp += 1 }.getOrElse { failed = true }
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
  def reconcile(): Boolean = {
    val (_, complete, prunedUnlisted) = sweep(reproject = true)
    verdict.reportUnlisted("reproject", prunedUnlisted, applied = _ => false)
    complete
  }

  /** Cheap id-only orphan prune — the frequent backstop for deleted / merged-away rows, and
   *  for the rows a silent change stream failed to deliver (see `sweep`). [[TickRun.Incomplete]] when the sweep's
   *  scan, a projection or the prune failed (logged by the sweep): the prune it skipped is not a completed run. */
  def pruneOrphans(): TickRun = {
    projection.exclusively(verdict.begin())
    val (healed, complete, rewritten, prunedUnlisted) =
      tools.AnyThrowable.rethrownAfter {
        val (healed, complete, prunedUnlisted) = sweep(reproject = false)
        projection.exclusively((healed, complete, verdict.rewritten, prunedUnlisted))
      }(_ => projection.exclusively(verdict.abandon()))
    verdict.verdictOnSweep(healed, rewritten, prunedUnlisted)
    TickRun.completedWhen(complete)
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
  private def sliceWalk(rowIds: Vector[services.movies.FilmId]): SliceWalk = new SliceWalk {
    def rows: Int = rowIds.size
    def reproject(slice: Int, scope: DerivationScope): (Int, Boolean) =
      if (scope == DerivationScope.Cards)
        reprojectSlice(rowIds, slice, "derivation pass (cards)",
                       movieRepository.findByIdWithSlotsChecked, row => projection.projectCards(projection.partition(row)))
      else
        reprojectSlice(rowIds, slice, "derivation pass", movieRepository.findByIdChecked, projection.projectRow(_, ProjectTrigger.Derivation))
  }

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

  // Hear of every change the projector applies, from now on (the verdict's `noteApplied`, `noteDrift`).
  projection.exclusively(projection.reportTo(verdict))
}

object ReadModelSweeper {
  /** What a sweep asks of the projector it sweeps: its lock, its per-row projection, and the memo
   *  of what it last wrote — which a sweep reads and corrects, since the read model, not the memo,
   *  is the truth. Every method but the stores, `exclusively` and `partition` expects the lock held. */
  trait Projection {
    /** The source store the projection reads. */
    def movieRepository: MovieRepositoryReader & MovieChangeFeed
    /** The read model it reads back (it writes only through the methods below). */
    def reader: ReadModelReader
    def metrics: ReadModelProjectionMetrics
    /** The clock it keeps time by — the rolling content check numbers its slice by it. */
    def clock: java.time.Clock
    /** Caller holds the lock. Tell `listener`, from now on, of each change the projector applies. */
    private[readmodel] def reportTo(listener: Listener): Unit
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
    /** Delete a screenings row a sweep found orphaned, and forget it. */
    def removeScreening(ref: ScreeningRef): Unit
  }

  /** What the projector tells the sweeper over it, under its lock: the rows the change stream applied
   *  or deleted, and the content check's rewrites. */
  private[readmodel] trait Listener {
    def noteApplied(rowId: String): Unit
    def noteDrift(row: RowDrift): Unit
  }
  private[readmodel] object Listener {
    /** No sweeper over the projector: nothing to tell. */
    val none: Listener = new Listener {
      def noteApplied(rowId: String): Unit = ()
      def noteDrift(row: RowDrift): Unit   = ()
    }
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

  /** The rows a complete prune sweep saw, walked one content slice at a time: what a derivation pass
   *  ([[ReadModelDerivationPass]]) re-projects, without knowing the rows or how they are read. */
  private[readmodel] trait SliceWalk {
    /** How many rows the walk covers. */
    def rows: Int
    /** Caller holds the projection lock. Re-project the rows in content slice `slice` — their cards alone for a
     *  `Cards` scope: the documents written, and whether every row was read and projected. */
    def reproject(slice: Int, scope: DerivationScope): (Int, Boolean)
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
    sleep:    scala.concurrent.duration.FiniteDuration => Unit,
    grace:    scala.concurrent.duration.FiniteDuration = InFlightDeliveryGrace,
    timeout:  scala.concurrent.duration.FiniteDuration = InFlightApplyTimeout
  ): Unit =
    if (ChangeStreamLiveness.Collections.exists(liveness.isWatching)) {
      sleep(grace)
      val handedOff = liveness.lastTicket
      // Bounded by polls, not a deadline: the check is a field read, so `timeout / AppliedPoll` polls wait `timeout`,
      // and a spec's sleep that skips the wait still ends the loop.
      val polls = math.max(1L, timeout.toNanos / AppliedPoll.toNanos)
      Iterator.range(0L, polls).takeWhile(_ => !liveness.appliedThrough(handedOff)).foreach(_ => sleep(AppliedPoll))
    }

  /** How often [[awaitStreamApplied]] looks again at what the apply thread has applied. */
  private val AppliedPoll = scala.concurrent.duration.Duration(50L, java.util.concurrent.TimeUnit.MILLISECONDS)

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
