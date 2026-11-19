package services.readmodel

import play.api.Logging
import services.movies.{ChangeStreamLiveness, MovieRepository, StoredMovieRecord}

/** THE SELF-HEAL FOR A SILENT CHANGE STREAM, run by each prune sweep ([[PruneSweep]]). A terminal
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
 *  prune must stay the id-only sweep it is there.
 *
 *  Logs as the sweeper it runs for. */
private[readmodel] final class SweepCatchUp(projection: ReadModelSweeper.Projection, override protected val logger: play.api.Logger)
    extends ContinuingSteps with Logging {
  import ReadModelProjectionMetrics.ProjectTrigger

  /** Caller holds the projection lock. Re-project what the `movies` cursor has not delivered. */
  def catchUpUndelivered(kind: String): Unit = {
    val liveness = projection.movieRepository.changeStreamLiveness
    if (liveness.isWatching(ChangeStreamLiveness.Movies)) {
      val readFrom = liveness.now()
      val pass = new CatchUpPass(kind, liveness.catchUpFloor(ChangeStreamLiveness.Movies), liveness.appliedThrough(liveness.lastTicket))
      if (pass.run().isComplete && !pass.failed) liveness.caughtUp(ChangeStreamLiveness.Movies, readFrom)
      projection.metrics.recordCatchUp(pass.caughtUp)
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
      projection.movieRepository.foreachRecordUpdatedSince(floor) { row => past += row.id.value; catchUp(row)(_ => true) }
        .andThen(projection.movieRepository.foreachRecordUpdatedSince(floor.minus(MovieRepository.CatchUpOverlap)) { row =>
          if (!past(row.id.value)) catchUp(row)(written => written > 0 && backlogApplied)
        })

    private def catchUp(row: StoredMovieRecord)(counted: Int => Boolean): Unit =
      if (row.record.readyToProject)
        continuing(s"read-model $kind: a row written since the change stream last delivered failed to project") {
          if (counted(projection.projectRow(row, ProjectTrigger.CatchUp))) caughtUp += 1 }.getOrElse { failed = true }
  }
}
