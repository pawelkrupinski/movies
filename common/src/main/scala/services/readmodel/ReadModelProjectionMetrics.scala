package services.readmodel

/**
 * Sink for read-model projection churn — how much the worker rewrites the
 * denormalised `web_movies` / `web_screenings` documents, and how often a whole
 * film is removed from the read model.
 *
 * Two signals, both fed from [[ReadModelProjector]]:
 *
 *  - `recordWrite` — one derived document (re)written or removed, by `target`
 *    (movie|screening) and `op` (upsert|delete). `rate()` is the reprojection
 *    volume the worker pushes through the web's two change streams.
 *  - `recordFilmPruned` — a film whose source row vanished or was *re-keyed* (its
 *    `filmId` changed) had all its derived documents dropped in `reconcile`. This
 *    is the event that can briefly 404 a film deep-link while the new key's
 *    documents propagate to the web (the web joins `web_movies`+`web_screenings`
 *    over two independent change streams, so it momentarily drops the film). Pair
 *    its rate with `kinowo_worker_merges_total` — the upstream re-key cause.
 *
 * The worker wires the Prometheus-backed [[services.metrics.WorkerTaskMetrics]];
 * the web and unit tests use [[ReadModelProjectionMetrics.noop]]. Mirrors
 * [[services.movies.MergeMetrics]].
 */
trait ReadModelProjectionMetrics {
  def recordWrite(target: String, op: String, count: Int): Unit
  def recordFilmPruned(count: Int): Unit
}

object ReadModelProjectionMetrics {
  /** Prometheus `target` label values for the writes counter. */
  object Target { val Movie = "movie"; val Screening = "screening" }
  /** Prometheus `op` label values for the writes counter. */
  object Op { val Upsert = "upsert"; val Delete = "delete" }

  val Targets: Seq[String] = Seq(Target.Movie, Target.Screening)
  val Ops:     Seq[String] = Seq(Op.Upsert, Op.Delete)

  val noop: ReadModelProjectionMetrics = new ReadModelProjectionMetrics {
    def recordWrite(target: String, op: String, count: Int): Unit = ()
    def recordFilmPruned(count: Int): Unit                        = ()
  }
}
