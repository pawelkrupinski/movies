package services.tasks

import tools.contracts.FailsOnPurpose

/** A [[ChunkScrapeStore]] whose READS throw while [[failingReads]] is set — the run marker,
 *  the stored keys, the chunk slices, the run list — and whose writes always land, so a
 *  spec can store a run's chunks and then blind only the reduce that reads them back. */
class FailingReadChunkScrapeStore extends InMemoryChunkScrapeStore with FailsOnPurpose {
  @volatile var failingReads: Boolean = false
  private def unreadable() = throw new RuntimeException("scrape_chunks unreadable")

  override def activeRun(cinema: String): Option[ChunkRun] =
    if (failingReads) unreadable() else super.activeRun(cinema)
  override def storedKeys(cinema: String, runId: String): Set[String] =
    if (failingReads) unreadable() else super.storedKeys(cinema, runId)
  override def loadChunks(cinema: String, runId: String): Map[String, String] =
    if (failingReads) unreadable() else super.loadChunks(cinema, runId)
  override def activeRuns(): Seq[ChunkRun] =
    if (failingReads) unreadable() else super.activeRuns()
}
