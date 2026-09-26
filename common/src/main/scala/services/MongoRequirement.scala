package services

/** What an absent or unreachable Mongo means for a [[MongoConnection]]: `Required` refuses a
 *  misconfigured boot and reconnects an unreachable one in the background; `Optional`
 *  disables the connection and degrades (repositories no-op, pages render film-less). */
enum MongoRequirement {
  case Required, Optional
}
