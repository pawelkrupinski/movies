package services.movies

import tools.RetryWithBackoff

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

/**
 * Keyset-paged scan of a whole Mongo collection by a unique, immutable `_id`-like
 * string key. Reads one `batchSize`-row page at a time — each a fresh, bounded,
 * independently-retried `find(_id > lastSeen).sort(_id).limit(batchSize)` — and hands
 * every batch to `onBatch`, rather than pulling the whole collection through ONE
 * unbounded `find().toFuture()`.
 *
 * Why paged, not one cursor: a single unbounded find over a large collection recurses
 * the async Mongo driver's per-message read-completion chain (`AsyncSupplier.finish` →
 * `AsyncCompletionHandler` → `SingleResultCallback.completed`) deep enough to throw
 * `StackOverflowError` on a driver I/O thread once the collection grows past a threshold
 * (Sentry KINOWO-19 on `movies`, then the same crash on `screenings`). Because the crash
 * lands on an uncaught I/O thread — not on the caller's `Await` — no `Try.recover` catches
 * it; it kills whatever boot/rehydrate path triggered the read. Keyset paging caps how
 * many rows any ONE cursor delivers synchronously, so the completion chain stays shallow.
 *
 * Exactly-once under concurrent writes: `fetchPage` must run a server-side
 * `_id > afterId` + sort-ascending + limit query against a unique, immutable `_id`, so a
 * concurrent write can neither resurface a visited row nor hide one — no duplicate at a
 * page boundary, no skip. `keyOf` extracts that `_id` from a decoded row.
 *
 * Returns `true` only when the scan reached the last page; `false` when a page still
 * failed after its retries. Rows delivered so far still reached `onBatch`, so a PRUNING
 * caller must treat `false` as "not the complete collection" and skip its destructive
 * step. `onIncomplete` is invoked once with the failure so the caller can log it.
 */
object KeysetScan {

  def scan[A](
    label:          String,
    batchSize:      Int,
    maxAttempts:    Int,
    initialBackoff: FiniteDuration,
    keyOf:          A => String,
    fetchPage:      (Option[String], Int) => Seq[A],
    onIncomplete:   Throwable => Unit = _ => ()
  )(onBatch: Seq[A] => Unit): Boolean =
    Try {
      var afterId: Option[String] = None
      var more                    = true
      while (more) {
        val batch = RetryWithBackoff(
          label          = label,
          maxAttempts    = maxAttempts,
          initialBackoff = initialBackoff
        )(fetchPage(afterId, batchSize))
        onBatch(batch)
        afterId = batch.lastOption.map(keyOf)
        more    = batch.sizeIs == batchSize
      }
      true
    }.recover { case exception: Throwable =>
      onIncomplete(exception)
      false
    }.getOrElse(false)
}
