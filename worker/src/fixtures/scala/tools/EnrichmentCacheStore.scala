package tools

/**
 * One remembered outcome of an enrichment HTTP call — a body, raw bytes, or the
 * FAILURE the call ended in.
 *
 * Failures are first-class on purpose. A convergence run's determinism depends on
 * the second pass seeing exactly what the first one saw, and for a good half of
 * a country's corpus what the first pass saw was "TMDB has never heard of this
 * title" (404) or "no Metacritic slug under that name". Remembering only the
 * successes would leave every one of those re-asking the live service on every
 * pass, which is both the slow half of the sweep and the half most likely to
 * answer differently the second time.
 */
sealed trait CachedResponse
object CachedResponse {
  final case class Body(text: String)                                           extends CachedResponse
  final case class Bytes(base64: String)                                        extends CachedResponse
  final case class Failed(status: Option[Int], method: String, message: String) extends CachedResponse
}

/**
 * Where an [[EnrichmentCache]]'s remembered responses live between runs.
 *
 * The seam is deliberately drawn at STORAGE and nowhere else: which URLs are worth
 * a key, how a failure is encoded and revived, when a hit counts as a hit — all of
 * that lives above this line in `EnrichmentCache` and `CachingEnrichmentFetch`, so
 * the Mongo store and the in-memory one cannot disagree about any of it. A store
 * only has to hand back what it was given.
 */
trait EnrichmentCacheStore {
  /** Every unexpired entry, read in ONE round-trip — the whole point of the
   *  preload (see [[EnrichmentCache.preload]]). */
  def loadAll(): Map[String, CachedResponse]

  def put(key: String, response: CachedResponse): Unit
}

/** The test double. Holds what it was given; no expiry, because a spec that wants
 *  to test expiry constructs the state it wants directly. */
class InMemoryEnrichmentCacheStore(initial: Map[String, CachedResponse] = Map.empty) extends EnrichmentCacheStore {
  private val entries    = new java.util.concurrent.ConcurrentHashMap[String, CachedResponse]()
  private val writeCount = new java.util.concurrent.atomic.AtomicInteger(0)

  initial.foreach { case (key, response) => entries.put(key, response) }

  /** Writes seen since construction, for a spec asserting the cache wrote THROUGH
   *  rather than merely answering from its own memory. */
  def writes: Int = writeCount.get()

  override def loadAll(): Map[String, CachedResponse] = {
    import scala.jdk.CollectionConverters._
    entries.asScala.toMap
  }

  override def put(key: String, response: CachedResponse): Unit = {
    entries.put(key, response)
    writeCount.incrementAndGet()
    ()
  }
}

/** A store nothing can reach — the CI shape where the cache URI names a tunnel that
 *  was never started, so the driver blocks for its server-selection timeout and then
 *  throws. Counts what it was ASKED, which is the number that matters: the point of
 *  the write circuit is that the count stops climbing. */
class UnreachableEnrichmentCacheStore(message: String = "no server available") extends EnrichmentCacheStore {
  private val attemptCount = new java.util.concurrent.atomic.AtomicInteger(0)

  def attempts: Int = attemptCount.get()

  override def loadAll(): Map[String, CachedResponse] = throw new RuntimeException(message)

  override def put(key: String, response: CachedResponse): Unit = {
    attemptCount.incrementAndGet()
    throw new RuntimeException(message)
  }
}

/** Fails its first `failFirst` writes and accepts everything after — the tunnel that
 *  drops and comes back, which is why the circuit reopens rather than latching. */
class IntermittentEnrichmentCacheStore(failFirst: Int) extends EnrichmentCacheStore {
  private val attemptCount = new java.util.concurrent.atomic.AtomicInteger(0)
  private val writeCount   = new java.util.concurrent.atomic.AtomicInteger(0)

  def attempts: Int = attemptCount.get()
  def writes: Int   = writeCount.get()

  override def loadAll(): Map[String, CachedResponse] = Map.empty

  override def put(key: String, response: CachedResponse): Unit =
    if (attemptCount.incrementAndGet() <= failFirst) throw new RuntimeException("store not ready")
    else writeCount.incrementAndGet()
}
