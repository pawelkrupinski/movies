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
}
