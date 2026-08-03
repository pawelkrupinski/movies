package tools

import play.api.Logging

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.concurrent.duration._
import scala.util.control.NonFatal

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
class EnrichmentCache(
  store: EnrichmentCacheStore,
  clock: () => Long = () => System.currentTimeMillis(),
  // False where a recording fixture tree is already writing every successful response
  // to disk, which makes a second copy here pure duplication: the fixtures are
  // consulted BEFORE the cache, so a URL the tree holds never reaches this lookup at
  // all. It tripled a country's tarball — the UK reached 434 MB — to store answers
  // nothing would ever read. What only the cache can hold is a verdict no response
  // ever arrived for, and those are still persisted either way.
  persistSuccesses: Boolean = true
) extends Logging {

  private val entries      = new ConcurrentHashMap[String, CachedResponse]()
  private val hitCount     = new AtomicInteger(0)
  private val fillCount    = new AtomicInteger(0)
  private val failureCount = new AtomicInteger(0)

  private val transientCount           = new AtomicInteger(0)
  private val consecutiveWriteFailures = new AtomicInteger(0)
  private val writesSuspendedUntil     = new AtomicLong(Long.MinValue)

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

  /**
   * Write through: the run's memory ALWAYS, and the store only when the answer is one
   * the next run should be held to.
   *
   * The distinction is between a verdict and a moment. A 404 from Rotten Tomatoes is a
   * fact about the URL — there is no such slug, and there won't be one tomorrow — and
   * remembering it across runs is most of what makes a warm run fast. A 429, a 503, a
   * Cloudflare 403 or a socket timeout is a fact about the instant it happened in.
   * Persisting one of those pins it as though it were an answer: every later run
   * replays "this film has no rating" without ever asking again, the corpus quietly
   * loses coverage, and nothing looks wrong — a remembered failure is indistinguishable
   * from a remembered verdict once it is in the store. Left unpersisted, the next run
   * simply asks again, which is the whole point of a transient error.
   *
   * It is still remembered IN MEMORY for the rest of this run. Three passes of the
   * order-independence test share one cache, and a URL that answered 429 to one pass
   * and 200 to another would surface as an order-dependence that isn't one — the exact
   * thing the shared cache exists to prevent.
   */
  def remember(key: String, response: CachedResponse): Unit = {
    entries.put(key, response)
    fillCount.incrementAndGet()
    response match {
      case _: CachedResponse.Failed => failureCount.incrementAndGet()
      case _                        => ()
    }
    if (worthPersisting(response)) writeThrough(key, response)
    else response match {
      // Counted only for a failure deliberately left retryable. A success skipped
      // because the fixture tree holds it isn't "not remembered" in any lossy sense,
      // and counting it here would read as a coverage problem.
      case _: CachedResponse.Failed => transientCount.incrementAndGet(); ()
      case _                        => ()
    }
  }

  /** Whether this outcome earns a place in the store: a failure only when its status is
   *  a verdict about the URL, a success only when nothing else is already recording it. */
  private def worthPersisting(response: CachedResponse): Boolean = response match {
    case failed: CachedResponse.Failed => EnrichmentCache.isDurable(failed)
    case _                             => persistSuccesses
  }

  /**
   * Write to the store, and STOP writing to one that keeps refusing.
   *
   * The write sits on the fetch path — `remember` runs inside the key's
   * single-flight lock — so its cost is paid per cache miss, serially. That is
   * fine against a store that answers in a millisecond and ruinous against one
   * that doesn't answer at all: pointed at a Mongo tunnel that was never started,
   * every write blocked for the driver's 5-second server selection before failing,
   * and three convergence legs were cancelled at the 75-minute ceiling having spent
   * the entire run waiting on a socket nobody was listening to. Thousands of
   * timeouts carry no more information than the first three.
   *
   * Suspended rather than latched off, because the tunnel these run over dies and
   * comes BACK: after the cooldown one write probes the store, and a success clears
   * the circuit entirely. So a fault that heals in seconds costs a minute of
   * write-through, not the rest of the run.
   *
   * The policy lives here, above the storage seam, so the Mongo store and the
   * in-memory one cannot disagree about it — and so a store only ever has to
   * report that it failed.
   */
  private def writeThrough(key: String, response: CachedResponse): Unit =
    if (clock() >= writesSuspendedUntil.get()) {
      try {
        store.put(key, response)
        if (consecutiveWriteFailures.getAndSet(0) > 0)
          logger.info("Enrichment cache store answered again — resuming write-through")
      } catch {
        case NonFatal(failure) =>
          if (consecutiveWriteFailures.incrementAndGet() >= EnrichmentCache.MaxConsecutiveWriteFailures) {
            writesSuspendedUntil.set(clock() + EnrichmentCache.WriteSuspension.toMillis)
            logger.warn(s"Enrichment cache write failed ${EnrichmentCache.MaxConsecutiveWriteFailures} times in a " +
              s"row (last: $key — ${failure.getMessage}) — suspending write-through for " +
              s"${EnrichmentCache.WriteSuspension.toSeconds}s; the run continues from memory")
          } else logger.warn(s"Enrichment cache write failed for $key: ${failure.getMessage}")
      }
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
    EnrichmentCache.Statistics(hitCount.get(), fillCount.get(), failureCount.get(), entries.size(),
      transientCount.get())
}

object EnrichmentCache {

  /**
   * The failure statuses that are a VERDICT about the URL rather than a report about
   * the moment, and so the only ones worth carrying to the next run.
   *
   * Deliberately a short allow-list rather than a list of things to exclude. A new
   * status nobody anticipated — a 451, a 425, whatever a CDN invents next — is far
   * more likely to be environmental than to be a permanent statement about a film,
   * and the cost of guessing wrong is asymmetric: an unremembered verdict costs one
   * live fetch on the next run, while a remembered non-verdict silently removes a
   * film's rating for as long as the entry lives.
   *
   * 404 is "no such slug", which is half a country's corpus. 410 is the same thing
   * said more definitely. 403 is NOT here: on these hosts it is a Cloudflare block,
   * which is exactly the kind of thing that clears by tomorrow.
   */
  /** Whether an outcome should outlive the run that saw it. Successes always;
   *  failures only when the status says something permanent about the URL. */
  def isDurable(response: CachedResponse): Boolean = response match {
    case CachedResponse.Failed(status, _, _) => status.exists(HttpStatusException.isDurable)
    case _                                   => true
  }

  /** How many consecutive write failures mean the store is GONE rather than
   *  momentarily busy. Three, because the information stops arriving after the
   *  first: a fourth `Connection refused` says exactly what the third one did, and
   *  costs another server-selection timeout to hear it. */
  val MaxConsecutiveWriteFailures = 3

  /** How long write-through stays suspended before one write probes the store
   *  again. A minute: long enough that a dead cluster costs a handful of timeouts
   *  across an hour-long sweep rather than one per miss, short enough that a tunnel
   *  which dropped and reconnected is picked back up within the same phase. */
  val WriteSuspension: FiniteDuration = 1.minute

  /** `transient` is the count of failures deliberately NOT carried to the next run.
   *  Reported because a run whose transient count is out of all proportion to its
   *  corpus was rate-limited, and that explains a coverage dip that would otherwise
   *  look like a resolver regression. */
  final case class Statistics(hits: Int, fills: Int, failures: Int, entries: Int, transient: Int = 0) {
    override def toString: String =
      s"$hits hits, $fills live fills ($failures failed, $transient transient — not remembered), " +
      s"$entries entries held"
  }
}
