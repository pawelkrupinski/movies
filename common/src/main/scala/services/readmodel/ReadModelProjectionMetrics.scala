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

  /** One `ReadModelProjection.projectAll` ran for a source row — `seconds` is its
   *  wall-clock (CPU-bound: no I/O in the pure projection). Fed from
   *  [[ReadModelProjector.project]]. `rate(..._duration_seconds_sum)` in centi-cores
   *  is the projection's share of worker CPU — the driver behind the periodic
   *  reproject burst and the boot-path credit floor; `..._calls_total` is how many
   *  rows were projected. The observability gap that made attributing that CPU hard. */
  def recordProject(seconds: Double): Unit

  /** One reconciliation sweep finished. `kind` distinguishes the cheap frequent
   *  orphan prune from the expensive periodic full re-projection; `didWork` is
   *  whether it pruned or reprojected at least one document. The point of the
   *  signal: a `reproject` sweep that is almost always `didWork=false` proves the
   *  change stream is reliable and the full sweep is redundant. */
  def recordReconcileSweep(kind: String, didWork: Boolean): Unit
}

object ReadModelProjectionMetrics {
  /** Prometheus `target` label values for the writes counter. */
  object Target { val Movie = "movie"; val Screening = "screening" }
  /** Prometheus `op` label values for the writes counter. */
  object Op { val Upsert = "upsert"; val Delete = "delete" }
  /** `kind` label values for the reconcile-sweep counter. */
  object ReconcileKind { val Prune = "prune"; val Reproject = "reproject" }

  val Targets: Seq[String]        = Seq(Target.Movie, Target.Screening)
  val Ops:     Seq[String]        = Seq(Op.Upsert, Op.Delete)
  val ReconcileKinds: Seq[String] = Seq(ReconcileKind.Prune, ReconcileKind.Reproject)

  val noop: ReadModelProjectionMetrics = new ReadModelProjectionMetrics {
    def recordWrite(target: String, op: String, count: Int): Unit = ()
    def recordFilmPruned(count: Int): Unit                        = ()
    def recordProject(seconds: Double): Unit                      = ()
    def recordReconcileSweep(kind: String, didWork: Boolean): Unit = ()
  }
}
