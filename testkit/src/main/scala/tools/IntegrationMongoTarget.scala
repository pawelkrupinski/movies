package tools

import settings.{MongoDatabaseName, MongoUri, ProcessConfiguration, RemoteIntegrationAllowed}

/**
 * The Mongo an integration run writes to: the throwaway cluster (`uri`), the database name
 * every suite's own database is prefixed with, and whether a credentialed (remote) cluster
 * was deliberately allowed ([[IntegrationMongo.OverrideVar]]).
 *
 * A value the spec — the root of its own run — resolves once from its configuration and
 * hands to the harness, so nothing in the harness reads the process for itself.
 */
final case class IntegrationMongoTarget(uri: MongoUri, databasePrefix: MongoDatabaseName, remoteAllowed: RemoteIntegrationAllowed) {

  /** Refuse a real cluster unless one was deliberately allowed — see [[IntegrationMongo]]. */
  def requireThrowaway(): Unit = IntegrationMongo.requireThrowaway(uri.value, remoteAllowed.value)
}

object IntegrationMongoTarget {

  /** `MONGODB_URI`, `MONGODB_DB` (default `kinowo`) and the override; None when no cluster is
   *  named. */
  def from(configuration: ProcessConfiguration): Option[IntegrationMongoTarget] = {
    val address = configuration.mongoAddress
    address.uri.map(uri => IntegrationMongoTarget(uri, address.database.getOrElse(MongoDatabaseName("kinowo")),
      configuration.remoteIntegrationAllowed))
  }
}
