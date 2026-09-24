package services.staging

/**
 * Sink for what the staging pipeline READS — the cost side the queue metrics cannot see,
 * because a kick that re-reads a film's whole staging group enqueues nothing new (the queue
 * dedups it) and so leaves no trace in `tasks_started`.
 *
 * `recordNewcomerKick` — one `StagingNewcomerDiverted` handled, with the number of staging
 * rows it decoded to decide the film's next step. A kick is due only for a film NEW to
 * staging, so its group is that venue's own row(s) and rows-per-kick sits near 1. Until
 * 2026-09-23 every venue JOINING an incubating film kicked too, and the k-th of N venues
 * decoded k rows: rows-per-kick of N/2, ~1,220 for a presale staged at 2,441 US venues, and
 * a scrape tick that went from 27s to 1,419s. `StagingNewcomerKickReadsWide` watches the ratio.
 *
 * The worker wires [[services.metrics.WorkerTaskMetrics]]; tests and the fixture harness
 * use [[StagingMetrics.noop]].
 */
trait StagingMetrics {
  def recordNewcomerKick(groupRows: Int): Unit
}

object StagingMetrics {
  val noop: StagingMetrics = (_: Int) => ()
}
