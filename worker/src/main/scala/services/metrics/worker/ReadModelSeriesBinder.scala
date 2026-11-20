package services.metrics.worker

import services.readmodel.ReadModelProjectionMetrics

/** One country's read-model projection metrics: every record bound to `countryCode` on the shared, registered-once
 *  [[ReadModelSeries]], so each country's wiring hands its projector the narrow [[ReadModelProjectionMetrics]] port. */
final class ReadModelSeriesBinder(countryCode: String, series: ReadModelSeries) extends ReadModelProjectionMetrics {
  def recordWrite(target: String, op: String, count: Int): Unit = series.recordWrite(countryCode, target, op, count)
  def recordFilmPruned(reason: String, count: Int): Unit        = series.recordFilmPruned(countryCode, reason, count)
  def recordCardRetired(reason: String): Unit                   = series.recordCardRetired(countryCode, reason)
  def recordDrift(cause: String, documents: Int): Unit          = series.recordDrift(countryCode, cause, documents)
  def recordUnlistedVenues(rows: Int, withheld: Boolean): Unit  = series.recordUnlistedVenues(countryCode, rows, withheld)
  def recordCatchUp(rows: Int): Unit                            = series.recordCatchUp(countryCode, rows)
  def recordCardWrite(changed: Set[String]): Unit               = series.recordCardWrite(countryCode, changed)
  def recordProject(trigger: ReadModelProjectionMetrics.ProjectTrigger, wallSeconds: Double, cpuSeconds: Double): Unit =
    series.recordProject(countryCode, trigger, wallSeconds, cpuSeconds)
  def recordWriteBurst(trigger: ReadModelProjectionMetrics.ProjectTrigger, seconds: Double): Unit =
    series.recordWriteBurst(countryCode, trigger, seconds)
  def recordMetadataProjection(trigger: ReadModelProjectionMetrics.ProjectTrigger, reused: Boolean): Unit =
    series.recordMetadataProjection(countryCode, trigger, reused)
  def recordVenueProjection(rebuilt: Int, reused: Int): Unit     = series.recordVenueProjection(countryCode, rebuilt, reused)
  def recordReconcileSweep(kind: String, didWork: Boolean): Unit = series.recordReconcileSweep(countryCode, kind, didWork)
  def recordHeal(trigger: String, rows: Int): Unit              = series.recordHeal(countryCode, trigger, rows)
}
