package tools

/**
 * The client reads that still swallow a failed read or parse into "no data" and have not
 * moved onto `tools.HttpRead` / `ReadOutcome` yet — the migration backlog for
 * [[NoSwallowedFailureSpec]]'s shape 3 and [[ClientReadsThroughHttpReadSpec]]. Every entry is
 * `TODO-HttpRead`: no reason is claimed for it, it is simply not migrated. Migrating a site
 * removes its entry (the specs' stale-entry checks insist); nothing new may be added here.
 *
 * What is left is TmdbClient's by-id details reads: switching them to throw shifts ~16 specs'
 * fakes and the e2e corpus, which lacks some detail fixtures the swallow hid (PARKED.md).
 */
object HttpReadBacklog {

  val Swallows: Map[(String, String, String), String] = Map(
    ("worker/src/main/scala/services/TmdbClient.scala", "details",
      "Try(httpGet(detailsUrl(tmdbId), auth))") -> "TODO-HttpRead",
    ("worker/src/main/scala/services/TmdbClient.scala", "fullDetails",
      "Try(httpGet(fullDetailsUrl(tmdbId), auth))") -> "TODO-HttpRead"
  )

  /** The client files that still call an `HttpFetch` directly — the migration backlog for
   *  [[ClientReadsThroughHttpReadSpec]]. Every entry is `TODO-HttpRead`. */
  val NotYetMigrated: Map[String, String] = Map(
    "worker/src/main/scala/services/TmdbClient.scala" -> "TODO-HttpRead"
  )
}
