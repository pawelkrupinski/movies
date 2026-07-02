package services.tasks

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}

import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

/**
 * A [[TaskQueue]] decorator that suppresses redundant `enqueue` round-trips with a
 * short-lived local cache of dedupKeys this instance believes are still active.
 *
 * The reapers re-scan and re-`enqueue` every due key each tick; a key already
 * waiting returns `Duplicate` from Mongo but STILL costs a full write round-trip
 * (and decode work on the driver's I/O threads — the worker's CPU sink). This
 * layer serves that repeat from memory instead.
 *
 * Multi-instance safety: Mongo's unique partial index on `dedupKey` stays the sole
 * authority, so the cache can only ever SKIP a round-trip, never create wrong
 * state. The one hazard is a stale positive — a key this instance cached but which
 * ANOTHER instance has since completed — which would suppress a genuinely-needed
 * re-enqueue. Two things bound it: the entry is evicted the moment THIS instance
 * completes the task, and `ttl` expires it regardless (so a cross-instance
 * completion self-heals within `ttl`). Handlers are freshness-gated and re-checked
 * every reaper tick, so a `ttl`-bounded delay is harmless. Keep `ttl` well under
 * the work cadence.
 */
class CachingTaskQueue(
  delegate: TaskQueue,
  ttl:      FiniteDuration = 3.minutes,
  maxKeys:  Long           = 100000L
) extends TaskQueue {

  // dedupKey -> present iff this instance enqueued it and hasn't seen it complete.
  private val active: Cache[String, java.lang.Boolean] =
    Caffeine.newBuilder().expireAfterWrite(ttl.toMillis, TimeUnit.MILLISECONDS).maximumSize(maxKeys).build()
  // taskId -> dedupKey, so complete(id) can evict the right `active` entry. Expires
  // on a lease-length scale so an id whose completion we never observe (reaped, or
  // finished on another instance) doesn't leak.
  private val idToKey: Cache[String, String] =
    Caffeine.newBuilder().expireAfterWrite(10L, TimeUnit.MINUTES).maximumSize(maxKeys).build()

  override def enqueue(taskType: TaskType, dedupKey: String, payload: Map[String, String], submittedAt: Instant): EnqueueResult =
    if (active.getIfPresent(dedupKey) != null) EnqueueResult.Duplicate // known-active: skip the Mongo round-trip
    else {
      val result = delegate.enqueue(taskType, dedupKey, payload, submittedAt)
      // Cache whether we ADDED it or found it already active in Mongo — either way a
      // re-enqueue before completion is a no-op we can now serve locally.
      active.put(dedupKey, java.lang.Boolean.TRUE)
      result
    }

  override def claim(workerId: String, lease: FiniteDuration, now: Instant): Option[Task] = {
    val task = delegate.claim(workerId, lease, now)
    task.foreach(t => idToKey.put(t.id, t.dedupKey))
    task
  }

  override def complete(id: String, workerId: String): Unit = {
    delegate.complete(id, workerId)
    // Finished — allow an immediate re-enqueue by dropping the active-key entry.
    Option(idToKey.getIfPresent(id)).foreach { key => idToKey.invalidate(id); active.invalidate(key) }
  }

  // A released task is still active (back to waiting) — keep its cache entry so we
  // don't re-enqueue it while it waits.
  override def release(id: String, workerId: String, error: Option[String], notBefore: Option[Instant]): Unit =
    delegate.release(id, workerId, error, notBefore)

  override def reapExpiredLeases(now: Instant): Int = delegate.reapExpiredLeases(now)
  override def countByState(): Map[String, Long] = delegate.countByState()
  override def waitingCount(taskType: TaskType): Int = delegate.waitingCount(taskType)
  override def monitor(activeLimit: Int): QueueSnapshot = delegate.monitor(activeLimit)
  override def watchWaiting(onWaiting: () => Unit): Option[AutoCloseable] = delegate.watchWaiting(onWaiting)
  override def close(): Unit = delegate.close()
}
