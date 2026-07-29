package tools

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

/**
 * A country's remembered enrichment answers, held in memory for the run and
 * written through to the [[EnrichmentCacheStore]] that outlives it.
 *
 * The memory lives HERE rather than inside [[CachingEnrichmentFetch]] because the
 * convergence suite runs several replays of the same corpus at once — the
 * order-independence test drives three concurrent passes — and they have to agree.
 * Each pass builds its own wiring and so its own fetch, but they share one cache;
 * a map per fetch would let three passes each fill the same URL from the live
 * service independently, and any disagreement between those three answers would
 * surface as an order-dependence that isn't one. Sharing the map makes the first
 * pass to ask the only one that asks.
 *
 * It is also the seam split the fakes rule asks for: every decision — what counts
 * as a hit, when to write through, what the statistics mean — is here, above the
 * storage boundary, so the Mongo store and the in-memory one cannot disagree about
 * any of it. A store only holds bytes.
 */
class EnrichmentCache(store: EnrichmentCacheStore) {

  private val entries      = new ConcurrentHashMap[String, CachedResponse]()
  private val hitCount     = new AtomicInteger(0)
  private val fillCount    = new AtomicInteger(0)
  private val failureCount = new AtomicInteger(0)

  /**
   * Pull the country's WHOLE cache into memory before the replay starts.
   *
   * One round-trip instead of a `findOne` per URL. A country's sweep asks tens of
   * thousands of enrichment questions, and on every run after the first the
   * answers are already known — so per-key lookups would spend the run paying
   * Mongo latency to be told what a single query could have said up front.
   *
   * Returns the number of entries loaded.
   */
  def preload(): Int = {
    val loaded = store.loadAll()
    loaded.foreach { case (key, response) => entries.put(key, response) }
    loaded.size
  }

  def lookup(key: String): Option[CachedResponse] = {
    val found = Option(entries.get(key))
    if (found.isDefined) hitCount.incrementAndGet()
    found
  }

  /** Write through: the run's memory AND the store, so the next run starts warm. */
  def remember(key: String, response: CachedResponse): Unit = {
    entries.put(key, response)
    fillCount.incrementAndGet()
    response match {
      case _: CachedResponse.Failed => failureCount.incrementAndGet()
      case _                        => ()
    }
    store.put(key, response)
  }

  /**
   * Run `work` with no other caller working on the SAME key at the same time.
   *
   * Concurrent replays of one corpus ask the same questions at the same moment.
   * Without this, two passes that both miss on a key put two calls on the wire, and
   * two live answers that disagree — a 429 for one and a 200 for the other is
   * enough — would surface as an order-dependent divergence that has nothing to do
   * with ordering. Whoever holds the lock fills; the others re-check and find a hit.
   *
   * A lock per key rather than one global lock: the fills are network-bound and
   * serialising all of them would turn a parallel sweep back into a serial one.
   */
  def singleFlight[A](key: String)(work: => A): A =
    locks.computeIfAbsent(key, _ => new Object).synchronized(work)

  private val locks = new ConcurrentHashMap[String, Object]()

  /** Hits, live fills, and how many of those fills FAILED — a run that got
   *  rate-limited shows up here as a failure count out of all proportion to the
   *  corpus, rather than as a quietly under-enriched read model nobody questions. */
  def statistics: EnrichmentCache.Statistics =
    EnrichmentCache.Statistics(hitCount.get(), fillCount.get(), failureCount.get(), entries.size())
}

object EnrichmentCache {
  final case class Statistics(hits: Int, fills: Int, failures: Int, entries: Int) {
    override def toString: String =
      s"$hits hits, $fills live fills ($failures failed), $entries entries held"
  }
}
