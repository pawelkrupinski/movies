package services.identity

import clients.TmdbClient
import play.api.Logging
import settings.IdentityShadowLookupRate
import services.cinemas.common.DetailEnricher
import services.movies.TitleNormalizer
import services.observations.{ObservationStore, ObservingHttpFetch}
import tools.{CircuitOpenException, HttpFetch, HttpStatusException}

import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.concurrent.duration._
import scala.util.control.NonFatal

/** One fill round's live asks: at most `allowance`, one per `pace`, and none after an overload.
 *  `deferred` counts the gaps it had no budget left for. Single-threaded by construction (a round
 *  is one resolve on one thread); the counters are atomic for the metrics reader. */
final class ShadowLookupBudget(allowance: Int, pace: FiniteDuration, sleep: Long => Unit) {
  private var used       = 0
  @volatile private var over = false
  val asked    = new AtomicInteger()
  val answered = new AtomicInteger()
  val failed   = new AtomicInteger()
  val deferred = new AtomicInteger()

  /** A slot for one live ask, waiting its pace; false once the allowance is spent or the round
   *  has backed off. */
  def acquire(): Boolean = synchronized {
    if (over || used >= allowance) { deferred.incrementAndGet(); false }
    else { if (used > 0) sleep(pace.toMillis); used += 1; asked.incrementAndGet(); true }
  }

  /** The service said stop (429, 5xx, a timeout, an open breaker): no more asks this round. */
  def backOff(): Unit = over = true
  def backedOff: Boolean = over
}

/** The fill's LIVE side: every request through `fetch` (the pipeline's shared lookup chain — its
 *  429 gate, breaker and meters — observed into the store, the only place its answer goes), each
 *  taking one slot of the round's `budget`. Without a slot it is deferred: an [[ObservationGap]],
 *  as unobserved as before. An overload ends the round's asks. */
final class ShadowLiveFetch(fetch: HttpFetch, budget: ShadowLookupBudget) extends HttpFetch {

  override def get(url: String): String = ask(url)(_.get(url))
  override def get(url: String, headers: Map[String, String]): String = ask(url)(_.get(url, headers))
  override def post(url: String, body: String, contentType: String): String = ask(url)(_.post(url, body, contentType))
  override def getBytes(url: String): Array[Byte] = ask(url)(_.getBytes(url))

  private def ask[A](url: String)(call: HttpFetch => A): A =
    if (!budget.acquire()) throw new ObservationGap(s"deferred: $url")
    else try { val a = call(fetch); budget.answered.incrementAndGet(); a }
    catch {
      case definitive: HttpStatusException if HttpStatusException.isDurable(definitive.code) =>
        budget.answered.incrementAndGet(); throw definitive
      case NonFatal(e) =>
        budget.failed.incrementAndGet()
        if (ShadowLiveFetch.isOverload(e)) budget.backOff()
        throw e
    }
}

object ShadowLiveFetch {
  /** The service asking us to slow down, or not answering: a 429, a 5xx, an open breaker, a
   *  network-level failure. */
  def isOverload(e: Throwable): Boolean = e match {
    case s: HttpStatusException          => s.code == 429 || s.code >= 500
    case _: CircuitOpenException         => true
    // A replayed fixture's miss (the network never throws it): permanent, as `TmdbClient` reads it.
    case _: java.io.FileNotFoundException => false
    case _: java.io.IOException          => true
    case _                               => false
  }
}

/** One fill round's outcome. */
final case class ShadowLookupRound(asked: Int, answered: Int, failed: Int, deferred: Int, gaps: Long, backedOff: Boolean,
                                   rate: IdentityShadowLookupRate)

/** Where the fill reports: its last round's asks, answers and deferrals. */
trait ShadowLookupMetrics {
  def round(r: ShadowLookupRound): Unit
}

object ShadowLookupMetrics {
  val noop: ShadowLookupMetrics = _ => ()
}

/**
 * The PACED LIVE LOOKUP FILL for the identity shadow run (docs/design/identity-resolver.md §19):
 * the shadow run answers only from the observation store, and most of the resolver's questions
 * (yearless searches, director walks, candidate records) are ones the pipeline never asks. After
 * each shadow tick, a round resolves the same listing set once more over the store — so the
 * questions are exactly the resolver's own (`CandidateQueries`, the query set
 * `IdentityLookupSweep` records), with no second list — and asks each unobserved TMDB question
 * live, at most `rate` per minute over the round's `window` (the shadow run's interval, so a
 * round's allowance is what fits before the next tick), through `liveFetch` (the pipeline's
 * shared lookup chain: its 429 gate, breaker and pace), filed ONLY into the store. The next tick
 * reads them. Nothing here writes a pipeline cache or row.
 *
 * Venue details are NOT asked: the pipeline's own detail refresh fetches every listing's page on
 * its own cadence (and the capture files it), while a shadow fetch would write the pipeline's
 * detail cache.
 *
 * Back-off: an overload (429, 5xx, open breaker, timeout) ends the round at once, and the next
 * round runs at half the rate; each clean round doubles it back, up to the configured rate. One
 * round at a time; a tick that finds one running starts none.
 */
final class ShadowLookupFill(
  listings:    () => Seq[Listing],
  store:       ObservationStore,
  tmdb:        HttpFetch => TmdbClient,
  liveFetch:   HttpFetch,
  enrichers:   Seq[DetailEnricher],
  normalizer:  TitleNormalizer,
  calibration: IdentityCalibration,
  rate:        => IdentityShadowLookupRate,
  window:      => settings.IdentityShadowInterval,
  metrics:     ShadowLookupMetrics,
  executor:    ExecutorService,
  sleep:       Long => Unit = Thread.sleep
) extends Logging {

  private val running = new AtomicBoolean(false)
  @volatile private var current: Option[IdentityShadowLookupRate] = None

  /** The rate the next round runs at: the configured one, less any back-off still in force. */
  def effectiveRate: IdentityShadowLookupRate = current.filter(_.perMinute < rate.perMinute).getOrElse(rate)

  /** One round, on the calling thread. */
  def round(): ShadowLookupRound = {
    val at     = effectiveRate
    val budget = new ShadowLookupBudget(at.allowanceOver(window.value), at.pace, sleep)
    // Observed first (main's `ObservedFirstHttpFetch`, the cut-over projection's own), live for a
    // gap within the budget; details from the store only.
    val gaps    = new ObservationGaps
    val lookups = new TmdbIdentityLookups(
      tmdb(new ObservedFirstHttpFetch(store, new ShadowLiveFetch(new ObservingHttpFetch(liveFetch, store), budget))),
      enrichers.map(new ObservedDetailEnricher(_, store, gaps)), () => gaps.total)
    try IdentityResolver.resolve(listings(), lookups, normalizer, calibration)
    catch { case crossing: IdentityResolver.FamilyCrossing => logger.warn(s"identity shadow fill: ${crossing.getMessage}") }
    current = Some(if (budget.backedOff) at.halved else IdentityShadowLookupRate((at.perMinute * 2).min(rate.perMinute)))
    val r = ShadowLookupRound(budget.asked.get, budget.answered.get, budget.failed.get, budget.deferred.get, gaps.total,
      budget.backedOff, at)
    metrics.round(r)
    logger.info(s"identity shadow fill: asked ${r.asked} (${r.answered} answered, ${r.failed} failed), deferred ${r.deferred} " +
      s"at ${at.perMinute}/min${if (r.backedOff) " — backed off" else ""}")
    r
  }

  /** Start a round in the background unless one is running. */
  def start(): Unit =
    if (running.compareAndSet(false, true))
      executor.execute { () =>
        try round() catch { case NonFatal(e) => logger.warn(s"identity shadow fill: round failed: $e") }
        finally running.set(false)
      }
}
