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
}

object CacheSyncMetrics {
  val noop: CacheSyncMetrics = new CacheSyncMetrics {
    def recordRehydrate(changedUpserts: Int, deletes: Int): Unit = ()
  }
}
