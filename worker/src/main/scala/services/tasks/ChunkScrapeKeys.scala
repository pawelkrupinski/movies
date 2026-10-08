package services.tasks

/** Payload field names + dedup-key builders for the chunked-scrape task family,
 *  in one place so the planner, handlers, coordinator and reaper can't disagree
 *  on the wire shape. A run is identified by `(cinema displayName, runId)`; a
 *  chunk additionally by its key. */
object ChunkScrapeKeys {
  val CinemaKey = "cinema"
  val RunIdKey  = "runId"
  val ChunkKey  = "chunk"

  /** One ScrapeChunk task per (cinema, run, key). The runId in the key means a
   *  superseding run's chunks never collapse onto a stale run's. */
  def chunkDedup(cinema: String, runId: String, key: String): String = s"chunk|$cinema|$runId|$key"

  /** The single ScrapeChunkReduce task for a run (so the coordinator + backstop
   *  enqueue it at most once). */
  def reduceDedup(cinema: String, runId: String): String = s"reduce|$cinema|$runId"

  def chunkPayload(cinema: String, runId: String, key: String): Map[String, String] =
    Map(CinemaKey -> cinema, RunIdKey -> runId, ChunkKey -> key)

  def reducePayload(cinema: String, runId: String): Map[String, String] =
    Map(CinemaKey -> cinema, RunIdKey -> runId)

  /** The run's record that the plan's day walk failed a read ([[services.cinemas.common.ListingReads]]):
   *  stored beside the slices, so the reduce publishes the listing as INCOMPLETE and the cache keeps
   *  the films that read lacked. Never an expected key: the run still completes when every chunk has
   *  stored. (A chunk's own failed read travels in its slice: [[StoredChunk.complete]].) Every key under
   *  the prefix reads as a marker, so one a previous release stored per chunk still counts. */
  private val IncompletePrefix = "incomplete|"
  val PlanIncomplete: String = IncompletePrefix + "plan"
  def isIncompleteMarker(key: String): Boolean = key.startsWith(IncompletePrefix)
}
