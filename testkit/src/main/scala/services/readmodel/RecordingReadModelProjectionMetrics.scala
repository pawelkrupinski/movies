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
  /** Each metadata projection's trigger, and whether it reused the cached metadata. */
  val metadataProjections = scala.collection.mutable.Buffer.empty[(ReadModelProjectionMetrics.ProjectTrigger, Boolean)]
  def metadataReused: Int     = metadataProjections.count(_._2)
  def metadataRecomputed: Int = metadataProjections.count(!_._2)
  val projectTriggers = scala.collection.mutable.Buffer.empty[ReadModelProjectionMetrics.ProjectTrigger]
  def projectCalls: Int = projectDurations.size
  def projectCalls(trigger: ReadModelProjectionMetrics.ProjectTrigger): Int = projectTriggers.count(_ == trigger)
  def recordWrite(target: String, op: String, count: Int): Unit = writes((target, op)) += count
  val pruneReasons = scala.collection.mutable.Buffer.empty[String]
  def recordFilmPruned(reason: String, count: Int): Unit        = { prunes += count; pruneReasons += reason }
  val retired = scala.collection.mutable.Buffer.empty[String]
  def recordCardRetired(reason: String): Unit                   = retired += reason
  /** Documents the content check rewrote, by what drifted. */
  val drift = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)
  def recordDrift(cause: String, documents: Int): Unit          = drift(cause) += documents
  /** Each sweep's unlisted venue rows, and whether they were withheld as over the cap. */
  val unlistedVenues = scala.collection.mutable.Buffer.empty[(Int, Boolean)]
  def recordUnlistedVenues(rows: Int, withheld: Boolean): Unit   = unlistedVenues += (rows -> withheld)
  def recordProject(trigger: ReadModelProjectionMetrics.ProjectTrigger, wallSeconds: Double, cpuSeconds: Double): Unit = {
    projectTriggers   += trigger
    projectDurations  += wallSeconds
    projectCpuSeconds += cpuSeconds
  }
  def recordWriteBurst(seconds: Double): Unit                   = writeBurstSeconds += seconds
  def recordMetadataProjection(trigger: ReadModelProjectionMetrics.ProjectTrigger, reused: Boolean): Unit =
    metadataProjections += (trigger -> reused)
  var venuesRebuilt = 0
  var venuesReused  = 0
  def recordVenueProjection(rebuilt: Int, reused: Int): Unit   = { venuesRebuilt += rebuilt; venuesReused += reused }
  def recordReconcileSweep(kind: String, didWork: Boolean): Unit = sweeps += (kind -> didWork)
  val caughtUp = scala.collection.mutable.Buffer.empty[Int]
  def recordCatchUp(rows: Int): Unit                             = caughtUp += rows
  val heals = scala.collection.mutable.Buffer.empty[(String, Int)]
  def recordHeal(trigger: String, rows: Int): Unit               = heals += (trigger -> rows)
  val cardWrites = scala.collection.mutable.Buffer.empty[Set[String]]
  def recordCardWrite(changed: Set[String]): Unit                = cardWrites += changed
}
