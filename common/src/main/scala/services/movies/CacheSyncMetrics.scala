package services.movies

/**
 * Sink for the MovieCache's periodic BACKSTOP rehydrate — how much the full-collection
 * reload catches that the incremental change stream (upserts via `applyUpsert`, deletes
 * via `applyDelete`) MISSED.
 *
 * The point of the signal: after resume-token persistence (restart gap-fill) + cache
 * delete-apply, the rehydrate should be a no-op in steady state — a `changed`/`deleted`
 * rate flat at ~0 proves the 30-min full reload is redundant and can be retired. (The
 * one-time BOOT hydrate legitimately counts every row as `changed`; exclude it by reading
 * the rate over steady state, not the raw counter.)
 *
 * Mirrors [[MergeMetrics]] / [[ReadModelProjectionMetrics]]: `noop` for web/tests, the
 * Prometheus-backed [[services.metrics.WorkerTaskMetrics]] in the worker.
 */
trait CacheSyncMetrics {
  def recordRehydrate(changedUpserts: Int, deletes: Int): Unit

  /** Phase-1 shadow for the index-only migration: at the `updated == before`
   *  write-guard (`MovieCache` putIfPresent), records whether a showtimes-DIGEST
   *  guard ([[ShowtimesDigest.leanEqual]]) would decide the same. Since `leanEqual`
   *  is true whenever `==` is true, the only possible disagreement is a FALSE SKIP
   *  (`leanEqual` true, `==` false) — a digest collision hiding a real showtime
   *  change. A false-skip rate flat at 0 over days gates flipping the guard. */
  def recordGuardShadow(fullEqual: Boolean, leanEqual: Boolean): Unit = ()
}

object CacheSyncMetrics {
  val noop: CacheSyncMetrics = new CacheSyncMetrics {
    def recordRehydrate(changedUpserts: Int, deletes: Int): Unit = ()
  }
}
