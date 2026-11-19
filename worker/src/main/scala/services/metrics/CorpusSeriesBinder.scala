package services.metrics

import services.movies.{CacheSyncMetrics, ChangeStreamMetrics, DecodeFailureMetrics, ListingIntakeMetrics, RepositoryWriteMetrics, ScreeningsMetrics, SideCollectionChangeMetrics}

/** One country's corpus metrics, each record bound to `countryCode` on the shared, registered-once series: the corpus
 *  writes ([[CorpusWriteSeries]] -- the cache's rehydrates, the scrape guards' verdicts, the repositories' failed writes
 *  and undecodable documents) and the change-stream cursors over `movies`, `screenings` and `movie_slots`
 *  ([[ChangeStreamSeries]]). This object is the writers' ports and the `screenings` cursor's; `movies` and `movie_slots`
 *  each get their own object -- `ChangeStreamMetrics.recordCoalescedChange` and `ScreeningsMetrics.recordCoalescedChange`
 *  erase to the same signature, and the slots cursor's trait has the screenings cursor's two methods, so one class
 *  cannot route them to different counters. */
final class CorpusSeriesBinder(countryCode: String, writes: CorpusWriteSeries, changeStreams: ChangeStreamSeries)
  extends CacheSyncMetrics with ListingIntakeMetrics with RepositoryWriteMetrics with DecodeFailureMetrics with ScreeningsMetrics {
  def recordRehydrate(changedUpserts: Int, deletes: Int): Unit = writes.recordRehydrate(countryCode, changedUpserts, deletes)
  def recordGuardVerdict(guard: String, verdict: String): Unit = writes.recordScrapeGuardVerdict(countryCode, guard, verdict)
  def recordWriteSkipped(reason: String): Unit                 = writes.recordScrapeWriteSkipped(countryCode, reason)
  def recordWriteFailed(collection: String, op: String, exception: String): Unit =
    writes.recordRepositoryWriteFailed(countryCode, collection, op, exception)
  def recordDecodeFailure(collection: String): Unit            = writes.recordDecodeFailure(countryCode, collection)

  def recordChangeEvent(op: String): Unit            = changeStreams.recordScreeningsChangeEvent(countryCode, op)
  def recordWrite(outcome: String, count: Int): Unit = changeStreams.recordScreeningsWrite(countryCode, outcome, count)
  def recordCoalescedChange(): Unit                  = changeStreams.recordScreeningsCoalesced(countryCode)

  /** The `movies` cursor's port. */
  val movies: ChangeStreamMetrics = new ChangeStreamMetrics {
    def recordEvent(op: String): Unit        = changeStreams.recordEvent(countryCode, op)
    def recordUpdateKind(kind: String): Unit = changeStreams.recordUpdateKind(countryCode, kind)
    def recordCoalescedChange(): Unit        = changeStreams.recordMovieCoalesced(countryCode)
    override def recordApply(path: String, reason: String): Unit = changeStreams.recordChangeApply(countryCode, path, reason)
    override def recordVenueDecline(reason: String): Unit       = changeStreams.recordVenueDecline(countryCode, reason)
  }

  /** The `movie_slots` cursor's port. */
  val slots: SideCollectionChangeMetrics = new SideCollectionChangeMetrics {
    def recordChangeEvent(op: String): Unit = changeStreams.recordSlotsChangeEvent(countryCode, op)
    def recordCoalescedChange(): Unit       = changeStreams.recordSlotsCoalesced(countryCode)
  }
}
