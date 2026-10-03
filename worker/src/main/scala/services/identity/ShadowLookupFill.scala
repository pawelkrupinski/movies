package services.identity

import clients.TmdbClient
import play.api.Logging
import settings.IdentityShadowLookupRate
import tools.{CircuitOpenException, HttpFetch, HttpStatusException}

import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.concurrent.duration._
import scala.util.control.NonFatal

/** One fill round's live asks: at most `allowance`, one per `pace`, and none to a host after it
 *  overloaded. `deferred` counts the gaps it had no budget left for. Single-threaded by
 *  construction (a round is one question walk on one thread); the counters are atomic for the metrics
 *  reader. */
final class ShadowLookupBudget(allowance: Int, pace: FiniteDuration, sleep: Long => Unit) {
  private var used       = 0
  private val overloaded = scala.collection.mutable.Set.empty[String]
  private val wanted     = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)
  val asked    = new AtomicInteger()
  val answered = new AtomicInteger()
  /** How many more asks this round's allowance has room for. */
  def remaining: Int = synchronized(allowance - used)
  val failed   = new AtomicInteger()
  val deferred = new AtomicInteger()

  /** A slot for one live ask to `host`, waiting its pace; false once the allowance is spent or
   *  the host has overloaded this round. */
  def acquire(host: String): Boolean = synchronized {
    wanted(host) += 1
    if (overloaded(host) || used >= allowance) { deferred.incrementAndGet(); false }
    else { if (used > 0) sleep(pace.toMillis); used += 1; asked.incrementAndGet(); true }
  }

  /** `host` said stop (429, 5xx, a timeout, an open breaker): no more asks to it this round. The
   *  other hosts' questions go on — one venue page timing out says nothing about TMDB. */
  def backOff(host: String): Unit = synchronized { overloaded += host }
  def backedOff: Boolean = synchronized { overloaded.nonEmpty }
  def overloadedHosts: Set[String] = synchronized { overloaded.toSet }

  /** Did a host that overloaded carry most of the round's questions (asked or deferred) — the
   *  service the rate paces? Then the next round runs slower; a minor host's overload is answered
   *  by skipping it, and by the pipeline chain's own per-host gate and breaker. */
  def paceOverloaded: Boolean = synchronized { overloaded.exists(h => wanted(h) * 2 >= wanted.values.sum) }
}

/** The fill's LIVE side: every request through `fetch` (the pipeline's shared lookup chain — its
 *  429 gate, breaker and meters — normalized into the model's TMDB store, the only place its answer
 *  goes), each taking one slot of the round's `budget`. Without a slot it is deferred: a
 *  [[LookupGap]], as unanswered as before. An overload ends the round's asks. */
final class ShadowLiveFetch(fetch: HttpFetch, budget: ShadowLookupBudget) extends HttpFetch with Logging {

  override def get(url: String): String = ask(url)(_.get(url))
  override def get(url: String, headers: Map[String, String]): String = ask(url)(_.get(url, headers))
  override def post(url: String, body: String, contentType: String): String = ask(url)(_.post(url, body, contentType))
  override def getBytes(url: String): Array[Byte] = ask(url)(_.getBytes(url))

  private def ask[A](url: String)(call: HttpFetch => A): A = {
    val host = ShadowLiveFetch.hostOf(url)
    if (!budget.acquire(host)) throw new LookupGap(s"deferred: $url")
    else try { val a = call(fetch); budget.answered.incrementAndGet(); a }
    catch {
      case definitive: HttpStatusException if HttpStatusException.isDurable(definitive.code) =>
        budget.answered.incrementAndGet(); throw definitive
      case NonFatal(e) =>
        budget.failed.incrementAndGet()
        val overload = ShadowLiveFetch.isOverload(e)
        // The host only: a URL may carry a key.
        logger.info(s"identity shadow fill: $host failed (${e.getClass.getSimpleName}${if (overload) ", overload" else ""})")
        if (overload) budget.backOff(host)
        throw e
    }
  }
}

object ShadowLiveFetch {
  def hostOf(url: String): String = scala.util.Try(java.net.URI.create(url).getHost).toOption.flatMap(Option(_)).getOrElse(url.takeWhile(_ != '?'))

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
 * the shadow run answers only from the model's normalized TMDB store, and most of the resolver's questions
 * (yearless searches, director walks, candidate records) are ones the pipeline never asks. After
 * each shadow tick, a round takes the identity model's GAPS (`IncrementalResolver.gaps`: the
 * questions its nodes asked that the store cannot answer, and the records it lacks) — so the
 * questions are exactly the resolver's own (`CandidateQueries`), with no second list — and asks each unobserved TMDB or IMDb
 * suggestion question live, at most `rate` per minute over the round's `window` (the shadow run's interval, so a
 * round's allowance is what fits before the next tick), through `liveFetch` (the pipeline's
 * shared lookup chain: its 429 gate, breaker and pace), filed ONLY into the store. The next tick
 * reads them. Nothing here writes a pipeline cache or row.
 *
 * Venue details are NOT asked: the pipeline's own detail enrichment reads every listing's page into
 * venue_pages on its own cadence, and the model reads it from there (`VenuePageIndex`).
 *
 * Back-off: an overload (429, 5xx, open breaker, timeout) ends the round at once, and the next
 * round runs at half the rate; each clean round doubles it back, up to the configured rate. One
 * round at a time; a tick that finds one running starts none.
 */
final class ShadowLookupFill(
  questions:   () => AnswersChanged,
  tmdb:        HttpFetch => TmdbClient,
  liveFetch:   HttpFetch,
  normalizer:  TmdbNormalizer,
  rate:        => IdentityShadowLookupRate,
  window:      => settings.IdentityShadowInterval,
  metrics:     ShadowLookupMetrics,
  executor:    ExecutorService,
  sleep:       Long => Unit = Thread.sleep,
  beforeRound: () => Unit = () => (),
  refreshes:   () => Seq[CandidateQuery] = () => Nil,
  gapMemory:   Option[TmdbGapMemory] = None
) extends Logging {

  private val running = new AtomicBoolean(false)
  @volatile private var current: Option[IdentityShadowLookupRate] = None

  /** The rate the next round runs at: the configured one, less any back-off still in force. */
  def effectiveRate: IdentityShadowLookupRate = current.filter(_.perMinute < rate.perMinute).getOrElse(rate)

  /** One round, on the calling thread. */
  def round(): ShadowLookupRound = {
    val at     = effectiveRate
    val budget = new ShadowLookupBudget(at.allowanceOver(window.value), at.pace, sleep)
    // Live within the budget, every answer normalized into the model's TMDB store — where the model's
    // gaps are, by definition, not yet.
    val gaps    = new LookupGaps
    val fetch   = new NormalizingHttpFetch(new ShadowLiveFetch(liveFetch, budget), normalizer)
    val lookups = new TmdbIdentityLookups(tmdb(fetch), new services.enrichment.ImdbClient(fetch), Nil, gaps)
    // Exactly the questions the identity model found unanswered, and the records it lacks — never
    // a walk of every listing's questions: the model knows its gaps. A record a newly answered
    // search names is the model's gap after its next drain, and the next round's question.
    // TMDB's edits first (`TmdbChangesSweep`, when one is due): what changed is fetched again
    // before the model's gaps and refreshes are chosen.
    try beforeRound() catch { case NonFatal(e) => logger.warn("identity shadow fill: TMDB changes not swept, this round", e) }
    val asked = gapMemory.fold(questions())(_.due(questions()))
    // Asked and still unanswered — not deferred for want of budget — is remembered, and asked again
    // a day later rather than every round (`TmdbGapMemory`).
    def stillUnanswered[A](answer: => Answer[A]): Boolean = {
      val deferred = budget.deferred.get
      !answer.isKnown && budget.deferred.get == deferred
    }
    val unansweredQueries = asked.queries.toSeq.sorted.filter(q => stillUnanswered(lookups.candidates(q)))
    val unansweredFilms   = asked.films.toSeq.sorted.filter(id => stillUnanswered(lookups.film(id)))
    gapMemory.foreach(_.unanswered(unansweredQueries, unansweredFilms))
    // Then, with what the allowance has left, questions asked again because they have aged
    // (`TmdbRefreshes`) — through the same live fetch.
    refreshes().iterator.takeWhile(_ => budget.remaining > 0).foreach(lookups.candidates)
    current = Some(if (budget.paceOverloaded) at.halved else IdentityShadowLookupRate((at.perMinute * 2).min(rate.perMinute)))
    val r = ShadowLookupRound(budget.asked.get, budget.answered.get, budget.failed.get, budget.deferred.get, gaps.total,
      budget.backedOff, at)
    metrics.round(r)
    logger.info(s"identity shadow fill: asked ${r.asked} (${r.answered} answered, ${r.failed} failed), deferred ${r.deferred} " +
      s"at ${at.perMinute}/min${if (r.backedOff) s" — backed off ${budget.overloadedHosts.toSeq.sorted.mkString(", ")}" else ""}")
    r
  }

  /** Start a round in the background unless one is running. A worker's shutdown interrupts a round
   *  mid-sleep or mid-fetch: that ends it, interrupt re-asserted, rather than escaping `NonFatal` as an
   *  uncaught exception Sentry reports as FATAL on every restart. */
  def start(): Unit =
    if (running.compareAndSet(false, true))
      executor.execute { () =>
        try round() catch {
          case _: InterruptedException => Thread.currentThread().interrupt()
          case NonFatal(e)             => logger.warn("identity shadow fill: round failed", e)
        }
        finally running.set(false)
      }
}
