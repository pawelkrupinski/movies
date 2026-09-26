package services

import settings.{MongoMaxPoolSize, MongoProbeTimeout, ProcessConfiguration}

/** How a [[MongoConnection]] dials — the boot probe's patience and the pool's size — as
 *  opposed to where ([[MongoAddress]]). Resolved once by a root and handed to every
 *  connection it opens. */
final case class MongoTuning(probeTimeout: MongoProbeTimeout, maxPoolSize: MongoMaxPoolSize)

object MongoTuning {

  /** The compiled-in defaults — what a spec or a process with nothing configured gets. */
  val Default: MongoTuning =
    MongoTuning(MongoProbeTimeout(MongoConnection.DefaultProbeTimeout), MongoMaxPoolSize(MongoConnection.DefaultMaxPoolSize))

  /** `MONGODB_PROBE_TIMEOUT_SECONDS` / `KINOWO_MONGO_MAX_POOL_SIZE`, else the defaults. */
  def from(configuration: ProcessConfiguration): MongoTuning =
    MongoTuning(configuration.mongoProbeTimeout(Default.probeTimeout), configuration.mongoMaxPoolSize(Default.maxPoolSize))
}
