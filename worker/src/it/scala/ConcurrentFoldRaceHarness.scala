package integration

import models.Cinema

import java.util.concurrent.CyclicBarrier
import scala.concurrent.duration._

/**
 * Fires N staging-fold GROUPS at real, genuinely-concurrent `MongoStagingFolder.foldGroup`
 * calls — a `CyclicBarrier` so every thread's session-open/read/write/commit actually
 * overlaps rather than queueing one after another (which a plain sequential call would
 * exercise instead, and would not reproduce the race at all).
 *
 * Extracted from `StagingFoldConcurrentTmdbRaceIntegrationSpec`, the first fold-race spec,
 * once a second race shape (see the three-way test in that file) needed the identical
 * seed/barrier/thread/join/collect plumbing — the threshold this repo extracts at is two
 * uses, not three.
 */
object ConcurrentFoldRaceHarness {

  /** One fold group to race: a cinema's staging row (seeded via `FoldFixture.Handles`)
   *  under its OWN `sanitize(title)` fold group, plus the `cleanTitle` argument
   *  `MongoStagingFolder.foldGroup` takes to fold exactly that group. */
  case class RaceGroup(cinema: Cinema, title: String, year: Option[Int], tmdbId: Int, imdbId: Option[String] = None)

  /** Seed every group's staging row, then fire all their `foldGroup` calls from separate
   *  threads blocked on one shared barrier until every thread has arrived — so the
   *  transactions genuinely overlap instead of one completing before the next starts.
   *  Returns each thread's outcome (success, or the exception it threw) in `groups`
   *  order, so the caller can assert on both the outcomes and the settled Mongo state
   *  afterwards (`fold.movies.find(...)`, `fold.slots.findForFilm(...)`, etc). */
  def race(fold: FoldFixture.Handles, groups: Seq[RaceGroup], joinTimeout: FiniteDuration = 30.seconds): Seq[Either[Throwable, Unit]] = {
    groups.foreach(g => fold.seedStagingRow(g.cinema.displayName, g.title, g.year, g.tmdbId, g.imdbId))
    val folder   = fold.folder()
    val barrier  = new CyclicBarrier(groups.size)
    val outcomes = Array.fill[Either[Throwable, Unit]](groups.size)(Left(new IllegalStateException("thread did not run")))
    val threads = groups.zipWithIndex.map { case (g, i) =>
      val t = new Thread(() => {
        barrier.await()
        outcomes(i) = try { folder.foldGroup(g.title); Right(()) } catch { case e: Throwable => Left(e) }
      })
      t.start()
      t
    }
    threads.foreach(_.join(joinTimeout.toMillis))
    outcomes.toSeq
  }
}
