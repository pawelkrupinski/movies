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

  /** One `ReadModelProjection.projectAll` ran for a source row. Fed from
   *  [[ReadModelProjector.project]]; `..._calls_total` is how many rows were projected.
   *
   *  Two costs, because they answer different questions and are NOT interchangeable:
   *
   *   - `wallSeconds` — how LONG the projection took. The latency signal: the duration
   *     histogram and its heatmap, where a bright high band means expensive films.
   *   - `cpuSeconds` — how much CPU it BURNED, from [[tools.ThreadCpuClock]]. The
   *     attribution signal: `rate(..._cpu_seconds_total)` in centi-cores is the
   *     projection's true share of worker CPU.
   *
   *  Only `cpuSeconds` may be compared against whole-process CPU. Wall-clock summed
   *  across concurrent projections can exceed the second it is measured in, and on a
   *  throttled box steal inflates it further — it read 45.9 centi-cores against an
   *  18.0 centi-core process total on `kinowo-worker-uk` (2026-07-28), which is what
   *  made it useless for the credit-floor diagnosis it was added for. */
  def recordProject(wallSeconds: Double, cpuSeconds: Double): Unit

  /** One projection decided whether to REUSE cached metadata or RECOMPUTE it.
   *  `reused=true` is the optimisation win: the row's metadata inputs were unchanged
   *  (a showtime-only change at an already-present cinema), so `resolve`/`synopsisByCity`/
   *  `ratingsFor` were skipped and only the cheap screenings half re-ran. `reused=false`
   *  is a genuine metadata change (rating / synopsis / new cinema) or a first projection.
   *  The rate of reused vs recomputed is the opt's effectiveness — high reuse under the
   *  showtime-churn the reproject/enrich pipeline generates is the whole point. */
  def recordMetadataProjection(reused: Boolean): Unit

  /** One orphan-prune sweep finished. `didWork` is whether it pruned at least one
   *  document — the deletes/re-keys the change stream can't deliver. (Only the prune
   *  is metered now; the full re-projection was retired, and with it its did_work
   *  gate.) `kind` is always `prune`; the label is kept for metric-shape stability. */
  def recordReconcileSweep(kind: String, didWork: Boolean): Unit
}

object ReadModelProjectionMetrics {
  /** Prometheus `target` label values for the writes counter. */
  object Target { val Movie = "movie"; val Screening = "screening" }
  /** Prometheus `op` label values for the writes counter. */
  object Op { val Upsert = "upsert"; val Delete = "delete" }
  /** `kind` label values for the reconcile-sweep counter. Only the prune is metered
   *  now (the full reproject was retired). */
  object ReconcileKind { val Prune = "prune" }
  /** `outcome` label values for the metadata-projection counter. */
  object MetadataOutcome { val Reused = "reused"; val Recomputed = "recomputed" }

  val Targets: Seq[String]        = Seq(Target.Movie, Target.Screening)
  val Ops:     Seq[String]        = Seq(Op.Upsert, Op.Delete)
  val ReconcileKinds: Seq[String] = Seq(ReconcileKind.Prune)
  val MetadataOutcomes: Seq[String] = Seq(MetadataOutcome.Reused, MetadataOutcome.Recomputed)

  val noop: ReadModelProjectionMetrics = new ReadModelProjectionMetrics {
    def recordWrite(target: String, op: String, count: Int): Unit = ()
    def recordFilmPruned(count: Int): Unit                        = ()
    def recordProject(wallSeconds: Double, cpuSeconds: Double): Unit = ()
    def recordMetadataProjection(reused: Boolean): Unit           = ()
    def recordReconcileSweep(kind: String, didWork: Boolean): Unit = ()
  }
}
