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
   *  afterwards (`fold.movies.find(...)`, `fold.slots.findForFilm(...)`, etc).
   *
   *  `maxRetries` defaults to `groups.size + 2`, not `MongoStagingFolder`'s production
   *  default of 3 — a `CyclicBarrier` start is more adversarial than anything production
   *  actually hits (every racer collides at the EXACT same instant, not just "close
   *  together"), so a losing racer can legitimately need close to `groups.size - 1`
   *  sequential retries to out-live every other racer, even with backoff jitter
   *  de-correlating most of them. Three attempts is the right number for the transient-
   *  error retry `MongoStagingFolder` actually ships with — a real duplicate-key or
   *  write-conflict collision against an already-committed sibling — but is too tight a
   *  budget for THIS harness's worst case: the 2026-09-10 CI run exhausted 3 attempts on
   *  a three-way race (two racers re-colliding with each other on the final attempt, pure
   *  bad luck in the jitter draw), which is a property of the test's adversarial setup,
   *  not a regression in the retry loop itself — see `MongoStagingFolder.retryBackoffMs`'s
   *  own doc comment for the design this budget has to survive. */
  def race(fold: FoldFixture.Handles, groups: Seq[RaceGroup], joinTimeout: FiniteDuration = 30.seconds,
    maxRetries: Option[Int] = None): Seq[Either[Throwable, Unit]] = {
    groups.foreach(g => fold.seedStagingRow(g.cinema.displayName, g.title, g.year, g.tmdbId, g.imdbId))
    val folder   = fold.folder(maxRetries = maxRetries.getOrElse(groups.size + 2))
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
