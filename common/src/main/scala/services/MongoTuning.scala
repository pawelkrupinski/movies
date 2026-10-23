package services

import settings.{MongoApplicationName, MongoMaxPoolSize, MongoOperationTimeout, MongoProbeTimeout, ProcessConfiguration, ProcessTier}

/** How a [[MongoConnection]] dials — the boot probe's patience, the pool's size, the
 *  `appName` it introduces itself by and the operation timeout every call inherits — as
 *  opposed to where ([[MongoAddress]]). Resolved once by a root and handed to every
 *  connection it opens. */
final case class MongoTuning(probeTimeout: MongoProbeTimeout, maxPoolSize: MongoMaxPoolSize,
    applicationName: MongoApplicationName, operationTimeout: Option[MongoOperationTimeout])

object MongoTuning {

  /** The compiled-in defaults — what a spec or a process with nothing configured gets. */
  val Default: MongoTuning =
    MongoTuning(MongoProbeTimeout(MongoConnection.DefaultProbeTimeout), MongoMaxPoolSize(MongoConnection.DefaultMaxPoolSize),
      MongoApplicationName.Default, Some(MongoOperationTimeout(MongoConnection.DefaultOperationTimeout)))

  /** `MONGODB_PROBE_TIMEOUT_SECONDS` / `KINOWO_MONGO_MAX_POOL_SIZE`, else the defaults, and the
   *  `appName` of a `tier` process serving the configured countries. The long-lived tiers get
   *  the default operation timeout; a one-off tool gets none — its bulk passes budget minutes
   *  for one write, and the person running it stops it by hand. */
  def from(configuration: ProcessConfiguration, tier: ProcessTier): MongoTuning =
    MongoTuning(configuration.mongoProbeTimeout(Default.probeTimeout), configuration.mongoMaxPoolSize(Default.maxPoolSize),
      configuration.mongoApplicationName(tier), if (tier == ProcessTier.Tool) None else Default.operationTimeout)
}
