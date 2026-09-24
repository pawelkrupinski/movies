package services.readmodel

/** A [[ReadModelProjectionMetrics]] that tallies every reprojection write, prune, retirement,
 *  sweep, heal and card write it is sent. */
final class RecordingReadModelProjectionMetrics extends ReadModelProjectionMetrics {
  val writes = scala.collection.mutable.Map.empty[(String, String), Int].withDefaultValue(0)
  var prunes = 0
  val sweeps = scala.collection.mutable.Buffer.empty[(String, Boolean)]
  val projectDurations = scala.collection.mutable.Buffer.empty[Double]
  val projectCpuSeconds = scala.collection.mutable.Buffer.empty[Double]
  val writeBurstSeconds = scala.collection.mutable.Buffer.empty[Double]
  var metadataReused = 0
  var metadataRecomputed = 0
  def projectCalls: Int = projectDurations.size
  def recordWrite(target: String, op: String, count: Int): Unit = writes((target, op)) += count
  val pruneReasons = scala.collection.mutable.Buffer.empty[String]
  def recordFilmPruned(reason: String, count: Int): Unit        = { prunes += count; pruneReasons += reason }
  val retired = scala.collection.mutable.Buffer.empty[String]
  def recordCardRetired(reason: String): Unit                   = retired += reason
  val driftWrites = scala.collection.mutable.Buffer.empty[Int]
  def recordDriftWrites(documents: Int): Unit                    = driftWrites += documents
  def recordProject(wallSeconds: Double, cpuSeconds: Double): Unit = {
    projectDurations  += wallSeconds
    projectCpuSeconds += cpuSeconds
  }
  def recordWriteBurst(seconds: Double): Unit                   = writeBurstSeconds += seconds
  def recordMetadataProjection(reused: Boolean): Unit          = if (reused) metadataReused += 1 else metadataRecomputed += 1
  var venuesRebuilt = 0
  var venuesReused  = 0
  def recordVenueProjection(rebuilt: Int, reused: Int): Unit   = { venuesRebuilt += rebuilt; venuesReused += reused }
  def recordReconcileSweep(kind: String, didWork: Boolean): Unit = sweeps += (kind -> didWork)
  val caughtUp = scala.collection.mutable.Buffer.empty[Int]
  def recordCatchUp(rows: Int): Unit                             = caughtUp += rows
  val heals = scala.collection.mutable.Buffer.empty[(String, Int)]
  def recordHeal(trigger: String, rows: Int): Unit               = heals += (trigger -> rows)
  val healChecks = scala.collection.mutable.Buffer.empty[(String, Int)]
  def recordHealCheck(trigger: String, rows: Int): Unit          = healChecks += (trigger -> rows)
  val cardWrites = scala.collection.mutable.Buffer.empty[Set[String]]
  def recordCardWrite(changed: Set[String]): Unit                = cardWrites += changed
}
