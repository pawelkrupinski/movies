package tools

/**
 * The Mongo an integration run writes to: the throwaway cluster (`uri`), the database name
 * every suite's own database is prefixed with, and whether a credentialed (remote) cluster
 * was deliberately allowed ([[IntegrationMongo.OverrideVar]]).
 *
 * A value the spec — the root of its own run — resolves once from its `Env` and hands to
 * the harness, so nothing in the harness reads the process for itself.
 */
final case class IntegrationMongoTarget(uri: String, databasePrefix: String, remoteAllowed: Boolean) {

  /** Refuse a real cluster unless one was deliberately allowed — see [[IntegrationMongo]]. */
  def requireThrowaway(): Unit = IntegrationMongo.requireThrowaway(uri, remoteAllowed)
}

object IntegrationMongoTarget {

  /** `MONGODB_URI`, `MONGODB_DB` (default `kinowo`) and the override, from `env`; None when
   *  no cluster is named. */
  def fromEnv(env: Env): Option[IntegrationMongoTarget] =
    env.get("MONGODB_URI").map(uri => IntegrationMongoTarget(uri, env.get("MONGODB_DB").getOrElse("kinowo"),
      remoteAllowed = env.get(IntegrationMongo.OverrideVar).exists(v => v == "1" || v.equalsIgnoreCase("true"))))
}
