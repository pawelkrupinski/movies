package services.lock

import scala.concurrent.duration.FiniteDuration

/**
 * Coarse-grained named lock for serialising work that must not run on more
 * than one app instance at a time — cinema scrape ticks, the daily TMDB
 * retry, the imdbId re-merge pass. Acquire is best-effort: if the lock can't
 * be claimed (held by another holder with unexpired TTL), `acquire` returns
 * false and the caller skips this iteration.
 *
 * TTL is what makes this safe in the face of crashes: a holder that dies
 * without releasing leaves an expired record that the next acquirer can take
 * over. Long-running work should `heartbeat` periodically so the lock
 * doesn't expire while still legitimately held.
 *
 * Per the project's deployment shape (Option A — single-leader writes,
 * multi-instance reads), this lock is what makes horizontal scale-out safe
 * for the write path. Today's single-instance deployment works regardless;
 * the lock costs nothing when there's no contention.
 */
trait Lock {
  /** Try to claim the named lock. Returns true on success.
   *  TTL is the maximum time the lock is held without a heartbeat. */
  def acquire(name: String, ttl: FiniteDuration): Boolean

  /** Extend the TTL on a currently-held lock. Returns false if this holder
   *  no longer owns the lock (lost it to a takeover after expiry, or was
   *  never holding). */
  def heartbeat(name: String, ttl: FiniteDuration): Boolean

  /** Release the lock if we hold it. No-op otherwise — releases by other
   *  holders are silently ignored so a stale `withLock` finally block can't
   *  yank a freshly-acquired lock out from under its new owner. */
  def release(name: String): Unit

  /** Run `action` while holding the lock; release at the end (even on
   *  exception). Returns Some(result) if the lock was acquired, None if a
   *  competing holder owned it. Does NOT heartbeat — for short critical
   *  sections only; long-running work should manage acquire/release plus
   *  heartbeat manually. */
  def withLock[A](name: String, ttl: FiniteDuration)(action: => A): Option[A] =
    if (acquire(name, ttl)) try Some(action) finally release(name)
    else None
}
