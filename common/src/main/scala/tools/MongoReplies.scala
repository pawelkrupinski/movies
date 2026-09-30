package tools

/**
 * How many documents one Mongo reply carries, per kind of document.
 *
 * A find or aggregate read to completion with `toFuture()` asks the server for batchSize =
 * Int.MaxValue — the reactive driver turns unbounded demand into the batch size — so every reply
 * fills to Mongo's 16 MB message cap, and the driver keeps a read buffer that size pooled for reuse.
 * worker-uk held 32 MB of idle pooled buffers (8/4/2/1 MB) in its live heap on 2026-09-29, from
 * per-film slot reads, observation scans and the identity families returning 9-16 MB replies.
 * Each bulk read asks for a batch sized so a reply stays near a megabyte or two; results are the
 * same, read in more round-trips. `NoUnboundedMongoReadSpec` holds every read to it.
 */
object MongoReplies {
  /** Rows around a kilobyte: slots, screenings, read-model rows, ids, small stores. */
  val Default: Int = 1000
  /** Stored films (`movies`, staging rows): ~25 KB each, a film's every slot. */
  val Films: Int = 50
  /** Identity families (`identity_model_families`): a region's decisions, tens of KB. */
  val Families: Int = 50
  /** Scrape archive rows (`cinema_scrapes`): one whole venue scrape, ~170 KB. */
  val ScrapeArchive: Int = 8
}
