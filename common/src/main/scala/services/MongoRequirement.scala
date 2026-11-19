package services

/** What an absent or unreachable Mongo means for a [[MongoConnection]] — the opening root's policy (a test, a local
 *  opt-out), not a setting of the connection: `Required` refuses a
 *  misconfigured boot and reconnects an unreachable one in the background; `Optional`
 *  disables the connection and degrades (repositories no-op, pages render film-less). */
enum MongoRequirement {
  case Required, Optional
}

object MongoRequirement {

  /** Pure policy core for the boot-failure decision: Mongo is required
   *  everywhere except tests, unless a local dev opted out (e.g. via
   *  `MONGODB_OPTIONAL`). Kept boolean-only — no Play `Mode`, no env reads —
   *  so the services layer doesn't depend on the framework and the rule
   *  stays trivially unit-testable. */
  def of(testMode: Boolean, optedOut: settings.MongoOptional): MongoRequirement =
    if (!testMode && !optedOut.value) Required else Optional
}
