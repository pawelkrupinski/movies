package services.readmodel

/** THE SWEEP'S VERDICT: which of a prune sweep's heals, content rewrites and unlisted-venue prunes were MISSES of the
 *  change stream, and which it merely had in flight. It hears, under the projection lock, each row the stream applies
 *  while a sweep runs ([[ReadModelSweeper.Listener]]) and the content check's rewrites; once the sweep lets go of the
 *  lock it waits for the stream to apply what it held (`awaitStreamApplied`, the sweeper's wait on its source's
 *  stream), then meters and names only the misses.
 *
 *  Built by the [[ReadModelSweeper]] over the projection it sweeps, and logging under the sweeper's `logger`: a
 *  verdict reads as its sweep's, which it is. */
private[readmodel] final class SweepVerdict(projection: ReadModelSweeper.Projection, awaitStreamApplied: () => Unit,
                                            logger: play.api.Logger) extends ReadModelSweeper.Listener {
  import ReadModelProjectionMetrics.{HealTrigger, ReconcileKind}
  private val metrics = projection.metrics

  // The rows the change stream applied or deleted since the running prune sweep started, `None`
  // outside one: what tells a heal the stream would have made anyway from a miss (`verdictOnSweep`).
  private var appliedSinceSweep: Option[scala.collection.mutable.Set[String]] = None
  // The rows the running sweep's content check rewrote, held for the same verdict as its heals:
  // only a row the stream does not then apply itself drifted (`verdictOnSweep`).
  private val sweepDrift = scala.collection.mutable.Buffer.empty[RowDrift]

  /** Caller holds the projection lock. The change stream applied (or deleted) `rowId`: a heal or a
   *  rewrite of it by the running sweep was in flight, not missed (`verdictOnSweep`). */
  def noteApplied(rowId: String): Unit = appliedSinceSweep.foreach(_ += rowId)

  /** Caller holds the projection lock. The content check rewrote a row that disagreed with its
   *  source: a drift candidate for the sweep's verdict. */
  def noteDrift(row: RowDrift): Unit = sweepDrift += row

  /** Caller holds the projection lock. A prune sweep starts: hear from now on what the stream applies. */
  def begin(): Unit = { appliedSinceSweep = Some(scala.collection.mutable.Set.empty); sweepDrift.clear() }

  /** Caller holds the projection lock. The sweep threw: hear no more until the next begins. */
  def abandon(): Unit = appliedSinceSweep = None

  /** Caller holds the projection lock. The rows the running sweep's content check rewrote. */
  def rewritten: Seq[RowDrift] = sweepDrift.toSeq

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
   *  slot written 38 s before the check rewrote it, the stream applying it once the sweep let go.
   *
   *  And so do its prunes of unlisted venues: 294 of the 321 rows pruned 2026-11-15..18 came within
   *  five minutes of their venue's refresh, all in the three debounced countries, each WARNed as "a
   *  removal the change stream did not apply" for a stream about to apply it. */
  def verdictOnSweep(healed: Seq[String], rewritten: Seq[RowDrift], prunedUnlisted: Seq[ScreeningRef]): Unit = {
    if (healed.nonEmpty || rewritten.nonEmpty || prunedUnlisted.nonEmpty) awaitStreamApplied()
    val applied = projection.exclusively { val rows = appliedSinceSweep.fold(Set.empty[String])(_.toSet); appliedSinceSweep = None; rows }
    val (raced, drifted) = rewritten.partition(row => applied(row.rowId))
    if (raced.nonEmpty)
      logger.info(s"read-model content check: rewrote ${raced.size} row(s) the change stream had in flight and then " +
        s"applied itself — not drift: ${ReadModelSweeper.idsForLog(raced.map(_.rowId))}.")
    drifted.foreach { row =>
      row.causes.foreach { case (cause, documents) => metrics.recordDrift(cause, documents) }
      logger.warn(s"read-model content check: row ${row.rowId} had drifted from its source — ${row.detail}.")
    }
    reportUnlisted(ReconcileKind.Prune, prunedUnlisted, applied)
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

  /** The unlisted venue rows a sweep pruned that the change stream did NOT apply itself once the
   *  sweep let go — `applied`, the rows it did — metered and named as missed removals. The rest it
   *  had in flight: a removal the debounce still held, or one queued behind the sweep's lock. */
  def reportUnlisted(kind: String, pruned: Seq[ScreeningRef], applied: String => Boolean): Unit = {
    val (inFlight, missed) = pruned.partition(ref => applied(ref.filmId.takeWhile(_ != '~')))
    if (inFlight.nonEmpty)
      logger.info(s"read-model $kind sweep: pruned ${inFlight.size} venue row(s) whose film no longer lists the venue " +
        s"and whose removal the change stream had in flight and then applied itself — not misses: " +
        s"${ReadModelSweeper.idsForLog(inFlight.map(_._id))}.")
    if (missed.nonEmpty) {
      metrics.recordUnlistedVenues(missed.size, withheld = false)
      logger.warn(s"read-model $kind sweep: pruned ${missed.size} venue row(s) whose film no longer lists the venue — " +
        s"a removal the change stream did not apply: ${ReadModelSweeper.idsForLog(missed.map(_._id))}.")
    }
  }
}
