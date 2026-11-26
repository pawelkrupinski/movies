package services.tasks

/** What one scrape of a cinema cost the task queue, in tasks: 1 for a plain scrape,
 *  the planner + one per chunk + the reduce for a chunked one. */
final case class ScrapeCost(tasks: Int) extends AnyVal

/**
 * The recent scrape costs of each cinema, keyed by its scrape dedup key — what
 * [[CostSpacedPhaseOffset]] spaces the scrape schedule by. Persisted, because a
 * country's scrape period (up to 14h) is longer than a worker usually stays up, so
 * costs learned in memory would be gone before they were ever used.
 */
trait ScrapeCostStore {
  /** Append one scrape's cost, keeping the latest [[ScrapeCostStore.RecentRuns]].
   *  Bookkeeping: a failed write is logged and dropped, never raised into a scrape. */
  def record(dedupKey: String, cost: ScrapeCost): Unit

  /** Append the cost of one scrape of `cinema`: `tasks` tasks, under its scrape dedup key. */
  final def recordScrape(cinema: models.Cinema, tasks: Int): Unit = record(ScrapeFreshnessPolicy.scrapeKey(cinema), ScrapeCost(tasks))

  /** Every recorded key's recent costs, oldest first. THROWS when unreadable — an
   *  unreadable store is not an empty one, and the caller keeps its last plan. */
  def recent(): Map[String, Seq[ScrapeCost]]
}

object ScrapeCostStore {
  /** How many recent scrapes a cinema's cost is averaged over. A venue's fan-out
   *  barely moves from one scrape to the next (median run-to-run variation 5-18% by
   *  country, 2026-09-21..28), so five is plenty to track a horizon that grows or
   *  shrinks, and few enough to follow it within a day. */
  val RecentRuns: Int = 5

  /** Records nothing — the default for callers that don't space by cost. */
  object Discarding extends ScrapeCostStore {
    def record(dedupKey: String, cost: ScrapeCost): Unit = ()
    def recent(): Map[String, Seq[ScrapeCost]] = Map.empty
  }
}

/** [[ScrapeCostStore]] in memory: tests, and a worker without Mongo. Each cinema's costs are kept as
 *  [[MongoScrapeCostStore]] stores them, in one keyed store over memory's documents. */
final class InMemoryScrapeCostStore extends KeyedScrapeCostStore(new tools.KeyedStore(new tools.InMemoryStoreDocuments(),
  tools.KeyCodec.Text, KeyedScrapeCostStore.Codec))
