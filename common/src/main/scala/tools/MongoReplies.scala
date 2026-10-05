package tools

/**
 * How many documents one Mongo reply carries, per kind of document.
 *
 * A find or aggregate read to completion with `toFuture()` asks the server for batchSize =
 * Int.MaxValue — the reactive driver turns unbounded demand into the batch size — so every reply
 * fills to Mongo's 16 MB message cap, and the driver keeps a read buffer that size pooled for reuse.
 * worker-uk held 32 MB of idle pooled buffers (8/4/2/1 MB) in its live heap on 2026-09-29, from
 * per-film slot reads, the (since removed) observation scans and the identity families returning 9-16 MB replies.
 * Each bulk read asks for a batch sized so a reply stays near a megabyte or two; results are the
 * same, read in more round-trips. `NoUnboundedMongoReadSpec` holds every read to it.
 */
object MongoReplies {
  /** Rows around a kilobyte: slots, read-model rows, ids, small stores. */
  val Default: Int = 1000
  /** `screenings` rows: a venue's showtimes with their booking links, ~2 KB each on a wide release (p90 3.6 KB) and up to
   *  65 KB on a presale's (worker-us mirror, 2026-10-05). A thousand made replies of 2-16 MB, read every couple of minutes
   *  by the re-reads of wide films; the driver's pool drops a buffer idle for a minute, so each made a new 2-16 MB buffer to
   *  be promoted and die in the old generation (prod JFR: 16 of 45 old-object samples). Over the US's 77 films of 300+
   *  venues, 150 rows keep 522 of 536 replies within the 1 MB class every read shares (1,000: 67 of 111). */
  val Screenings: Int = 150
  /** Stored films (`movies`, staging rows): ~25 KB each, a film's every slot. */
  val Films: Int = 50
  /** Identity families (`identity_model_families`): a region's decisions, tens of KB. */
  val Families: Int = 50
  /** Scrape archive rows (`cinema_scrapes`): one whole venue scrape, ~170 KB. */
  val ScrapeArchive: Int = 8
}
