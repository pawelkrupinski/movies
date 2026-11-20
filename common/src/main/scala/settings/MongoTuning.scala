package settings

import scala.concurrent.duration.*

/** How a [[services.MongoConnection]] dials — the boot probe's patience, the pool's size and the
 *  `appName` it introduces itself by — as opposed to where ([[MongoAddress]]). Resolved once
 *  by a root and handed to every connection it opens. */
final case class MongoTuning(probeTimeout: MongoProbeTimeout, maxPoolSize: MongoMaxPoolSize,
    applicationName: MongoApplicationName)

object MongoTuning {

  // Declared before `Default`, which reads them: an object's vals initialise in order.

  /** Boot connectivity-probe timeout. Raised from a hard-coded 10s after the
   *  2026-06-06 incident: the self-hosted Mongo went unresponsive under memory
   *  pressure (it OOM-killed minutes later), the 10s `countDocuments` probe
   *  timed out, the web process exited 255 → crash-looped → Fly hit the
   *  max-restart-count and stopped the machine. 30s rides out a momentarily
   *  slow or freshly-restarted Mongo. Override with
   *  `MONGODB_PROBE_TIMEOUT_SECONDS`. */
  val DefaultProbeTimeout: FiniteDuration = 30.seconds

  /** Connections per `MongoClient`. Deliberately far below the driver's default of
   *  100: every app holds its own pool against ONE small shared mongod, so the default
   *  lets a handful of clients reserve the whole box. 25 still leaves ample headroom —
   *  the worker's background concurrency and the web's request threads are both well
   *  under it — while capping the fleet's worst case to something the VM can hold.
   *  Tunable without a code change (same pattern as the credit thresholds) because
   *  it is a capacity knob we may need to move under pressure (`KINOWO_MONGO_MAX_POOL_SIZE`). */
  val DefaultMaxPoolSize: Int = 25

  /** The compiled-in defaults — what a spec or a process with nothing configured gets. */
  val Default: MongoTuning =
    MongoTuning(MongoProbeTimeout(DefaultProbeTimeout), MongoMaxPoolSize(DefaultMaxPoolSize), MongoApplicationName.Default)

  /** `MONGODB_PROBE_TIMEOUT_SECONDS` / `KINOWO_MONGO_MAX_POOL_SIZE`, else the defaults, and the
   *  `appName` of a `tier` process serving the configured countries. */
  def from(configuration: ProcessConfiguration, tier: ProcessTier): MongoTuning =
    MongoTuning(configuration.mongoProbeTimeout(Default.probeTimeout), configuration.mongoMaxPoolSize(Default.maxPoolSize),
      configuration.mongoApplicationName(tier))
}
