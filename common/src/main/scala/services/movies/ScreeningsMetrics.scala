package services.movies

/**
 * Observability sink for the `screenings` STORE — the second change stream, and the
 * one that actually drives most of the read-model projection.
 *
 * WHY THIS EXISTS. `ReadModelProjector` is fed by TWO cursors: `movies` (metered by
 * [[ChangeStreamMetrics]]) and `screenings` (metered by nothing until this). The
 * screenings cursor rings once per changed screenings DOCUMENT — one per (film, slot) —
 * and each ring costs a blocking stitch read plus a full projection, so it is the larger
 * of the two triggers by an order of magnitude. On 2026-09-04 `worker-de` logged 184
 * `movies` events against 10,114 projections in 85 minutes; the dashboard's "projection
 * trigger" line, which plots only the `movies` counter, was flat through the whole
 * episode. A trigger nothing counts is a trigger nobody can rule in or out.
 *
 *  - `recordChangeEvent(op)` — one screenings change-stream event, by operation. Added to
 *    the `movies` rate this is the projector's REAL input rate, and the denominator
 *    `readmodel_project_calls_total` should be read against. Projections outrunning the
 *    sum of both cursors would be the projector re-entering itself — a different bug, and
 *    one that was indistinguishable from this one while half the input was invisible.
 *  - `recordWrite(outcome, count)` — one attempted slot write, `written` or `unchanged`.
 *    The REDUNDANT-WRITE CANARY, and the counterpart of `ChangeStreamMetrics`'s
 *    `updated_at_only`: `unchanged` is a write the store recognised as a no-op and dropped, so
 *    it never reached the oplog, never rang the stream and never bought a projection. A high
 *    `unchanged` SHARE is not a fault — it is the guard earning its keep, and it is the number
 *    that names this class of problem in one look. It read 297:1 on prod on 2026-09-04, where
 *    every other signal said only "projections are up".
 *
 * The worker wires the Prometheus-backed [[services.metrics.WorkerTaskMetrics]]; the web,
 * scripts and unit tests use [[ScreeningsMetrics.noop]]. Mirrors [[ChangeStreamMetrics]]
 * and [[services.readmodel.ReadModelProjectionMetrics]].
 */
trait ScreeningsMetrics {
  def recordChangeEvent(op: String): Unit
  def recordWrite(outcome: String, count: Int): Unit
}

object ScreeningsMetrics {
  /** `outcome` label values for the writes counter. The OP vocabulary is
   *  [[ChangeStreamMetrics.Ops]]'s — one change stream's operations are the other's, and a
   *  second copy of that list would drift from it. */
  object Outcome { val Written = "written"; val Unchanged = "unchanged" }
  val Outcomes: Seq[String] = Seq(Outcome.Written, Outcome.Unchanged)

  val noop: ScreeningsMetrics = new ScreeningsMetrics {
    def recordChangeEvent(op: String): Unit            = ()
    def recordWrite(outcome: String, count: Int): Unit = ()
  }
}
