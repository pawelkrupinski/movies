package services.lookupfill

import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import scala.concurrent.duration.*
import scala.util.control.NonFatal

import clients.TmdbClient
import play.api.Logging
import services.identity.*
import services.identity.vocabulary.{AnswersChanged, Question}
import settings.IdentityLookupFillRate
import tools.{CircuitOpenException, ForwardingHttpFetch, HttpFetch, HttpStatusException, RequestedHost}

/** One fill round's live asks: at most `allowance`, one per `pace`, and none to a host after it
 *  overloaded. `deferred` counts the gaps it had no budget left for. Single-threaded by
 *  construction (a round is one question walk on one thread); the counters are atomic for the metrics
 *  reader. */
final class LookupFillBudget(allowance: Int, pace: FiniteDuration, sleep: Long => Unit) {
  private var used       = 0
  private val overloaded = scala.collection.mutable.Set.empty[String]
  // Each host's DISTINCT requests: one asked again (a title's IMDb suggestions, read both as the films IMDb suggests
  // and as those it lists under the title) is one question, deferred or not.
  private val wanted     = scala.collection.mutable.Map.empty[String, scala.collection.mutable.Set[String]]
  val asked    = new AtomicInteger()
  val answered = new AtomicInteger()
  /** How many more asks this round's allowance has room for. */
  def remaining: Int = synchronized(allowance - used)
  /** Asks whose read FAILED — taught nothing (a 5xx, a timeout, a 429, an open breaker); not a 404. */
  val failed   = new AtomicInteger()
  val deferred = new AtomicInteger()
  // each host's reads this round: answered, failed, deferred
  private val byHost = scala.collection.mutable.Map.empty[String, Array[Int]]
  @volatile private var failure: Option[Throwable] = None
  private def count(host: String, outcome: Int): Unit = byHost.getOrElseUpdate(host, new Array[Int](3))(outcome) += 1

  /** A read of `host` answered — a 404/410 too: the host's answer. */
  def answeredBy(host: String): Unit = synchronized { answered.incrementAndGet(); count(host, 0) }
  /** A read of `host` failed with `e`. */
  def failedAt(host: String, e: Throwable): Unit = synchronized { failed.incrementAndGet(); count(host, 1); failure = Some(e) }
  /** The round's latest failed read, what names the host a failed question's read went to. */
  def lastFailure: Option[Throwable] = failure
  /** How each host's wanted reads came out this round: (answered, failed, deferred). */
  def hosts: Map[String, (Int, Int, Int)] = synchronized(byHost.map { case (host, c) => host -> (c(0), c(1), c(2)) }.toMap)

  /** A slot for one live ask of `url` at `host`, waiting its pace; false once the allowance is spent or
   *  the host has overloaded this round. */
  def acquire(host: String, url: String): Boolean = synchronized {
    wanted.getOrElseUpdate(host, scala.collection.mutable.Set.empty) += url
    if (overloaded(host) || used >= allowance) { deferred.incrementAndGet(); count(host, 2); false }
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
  def paceOverloaded: Boolean = synchronized {
    overloaded.exists(h => wanted.get(h).fold(0)(_.size) * 2 >= wanted.values.map(_.size).sum)
  }
}

/** The fill's LIVE side: every request through `fetch` (the pipeline's shared lookup chain — its
 *  429 gate, breaker and meters — normalized into the model's TMDB store, the only place its answer
 *  goes), each taking one slot of the round's `budget`. Without a slot it is deferred: a
 *  [[LookupGap]], as unanswered as before. An overload ends the round's asks. */
final class LookupFillLiveFetch(fetch: HttpFetch, budget: LookupFillBudget) extends ForwardingHttpFetch with Logging {

  protected def around[A](url: String)(call: HttpFetch => A): A = {
    val host = LookupFillLiveFetch.hostOf(url)
    if (!budget.acquire(host, url)) throw new LookupGap(s"deferred: $url")
    else try { RequestedHost.note(host); val a = call(fetch); budget.answeredBy(host); a }
    catch {
      // The one classifier: a 404/410 is TMDB's answer ("nothing here"); everything else taught nothing.
      case absent if tools.ReadOutcome.isAbsent(absent) =>
        budget.answeredBy(host); throw absent
      case NonFatal(e) => failedAt(host, e); throw e
    }
  }

  /** A read of `host` that taught nothing (`e`), counted -- and an overload backed off: no more asks to it this round. */
  private def failedAt(host: String, e: Throwable): Unit = {
    budget.failedAt(host, e)
    val overload = LookupFillLiveFetch.isOverload(e)
    // The host only: a URL may carry a key.
    logger.info(s"identity lookup fill: $host failed (${e.getClass.getSimpleName}${if (overload) ", overload" else ""})")
    if (overload) budget.backOff(host)
  }
}

object LookupFillLiveFetch {
  /** Lower-cased, as the host breakers and the question memory name it. */
  def hostOf(url: String): String =
    tools.UrlHost.of(url).getOrElse(url.takeWhile(_ != '?').toLowerCase(java.util.Locale.ROOT))

  /** The service asking us to slow down, or not answering: a 429, a 5xx, an open breaker, a
   *  network-level failure. */
  def isOverload(e: Throwable): Boolean = e match {
    case s: HttpStatusException          => s.code == 429 || s.code >= 500
    case _: CircuitOpenException         => true
    // A replayed fixture's miss (the network never throws it): permanent, as `RetryingHttpFetch` reads it.
    case _: java.io.FileNotFoundException => false
    case _: java.io.IOException          => true
    case _                               => false
  }
}

/** A fill round's lookups, built over its `budget` ([[over]]): every request through `liveFetch` (the pipeline's shared
 *  lookup chain) within that budget ([[LookupFillLiveFetch]]), its answer normalized into the model's TMDB store
 *  (`normalizer`, [[NormalizingHttpFetch]]), asked by TMDB (`tmdb` over that fetch) and IMDb
 *  ([[TmdbIdentityLookups.live]]), each request the store could not answer counted in the round's gaps. */
final class LookupFillChain(tmdb: HttpFetch => TmdbClient, liveFetch: HttpFetch, normalizer: TmdbNormalizer) {
  /** One round's TMDB client and lookups over `budget`, and its gaps. */
  def over(budget: LookupFillBudget): LookupFillChain.Round = {
    val gaps   = new LookupGaps
    val fetch  = new NormalizingHttpFetch(new LookupFillLiveFetch(liveFetch, budget), normalizer)
    val client = tmdb(fetch)
    LookupFillChain.Round(client, TmdbIdentityLookups.live(client, fetch, gaps), gaps)
  }
}

object LookupFillChain {
  /** One round's TMDB client (what its renewals read again), its lookups and its gaps. */
  final case class Round(tmdb: TmdbClient, lookups: TmdbIdentityLookups, gaps: LookupGaps)
}

/** One fill round's outcome. */
final case class LookupFillRound(asked: Int, answered: Int, failed: Int, deferred: Int, gaps: Long, backedOff: Boolean,
                                   rate: IdentityLookupFillRate)

/** Where the fill reports: its last round's asks, answers and deferrals. */
trait LookupFillMetrics {
  def round(r: LookupFillRound): Unit
}

object LookupFillMetrics {
  val noop: LookupFillMetrics = _ => ()
}

/**
 * The PACED LIVE LOOKUP FILL of the identity model (docs/design/identity-resolver.md §19): the model
 * answers only from its normalized TMDB store, and most of the resolver's questions (yearless
 * searches, director walks, candidate records) are ones nothing else asks. Each round takes the
 * model's GAPS (`IncrementalResolver.gaps`: the questions its nodes asked that the store cannot
 * answer, and the records it lacks) — so the questions are exactly the resolver's own
 * (`CandidateQueries`), with no second list — and asks each unanswered TMDB or IMDb suggestion
 * question live, at most `rate` per minute over the round's `window` (the fill's interval, so a
 * round's allowance is what fits before the next), through `chain` (the shared lookup chain:
 * its 429 gate, breaker and pace), filed ONLY into the store, which the model then reads. Which gaps are due, and what
 * each round's asks leave in the question memory, is its `ledger`'s ([[LookupFillLedger]]).
 *
 * Venue details are NOT asked: the pipeline's own detail enrichment reads every listing's page into
 * venue_pages on its own cadence, and the model reads it from there (`VenuePageIndex`).
 *
 * Back-off: an overload (429, 5xx, open breaker, timeout) ends the round at once, and the next
 * round runs at half the rate; each clean round doubles it back, up to the configured rate. One
 * round at a time; a tick that finds one running starts none.
 */
final class LookupFill(
  questions:   () => AnswersChanged,
  chain:       LookupFillChain,
  rate:        => IdentityLookupFillRate,
  window:      => settings.IdentityLookupFillInterval,
  metrics:     LookupFillMetrics,
  executor:    ExecutorService,
  sleep:       Long => Unit,
  renewals:    () => Seq[Question],
  // What the fill keeps of its questions: which gaps are due, and what each round's asks came to.
  ledger:      LookupFillLedger,
  clock:       java.time.Clock,
  // Where each round that ran to its end is reported (`ScheduledTickStale`, tick [[LookupFill.Tick]]): its schedule
  // ([[LookupFillSchedule]]) only hands a round to `executor`, so a round that hangs or fails every time is seen here or nowhere.
  ticks:       tools.TickRecorder
) extends Logging {
  // When the last round began: a round's allowance is the rate over the time since, never more than a `window` —
  // rounds run as the model finds gaps (`EventTrigger`), not only once a window, and must not ask more for it.
  @volatile private var lastRound: Option[java.time.Instant] = None

  private val running = new AtomicBoolean(false)
  @volatile private var current: Option[IdentityLookupFillRate] = None

  /** The rate the next round runs at: the configured one, less any back-off still in force. */
  def effectiveRate: IdentityLookupFillRate = current.filter(_.perMinute < rate.perMinute).getOrElse(rate)

  /** One round, on the calling thread. */
  def round(): LookupFillRound = {
    val at     = effectiveRate
    val now    = clock.instant()
    val span   = lastRound.fold(window.value)(was => FiniteDuration(java.time.Duration.between(was, now).toMillis.max(0L),
      java.util.concurrent.TimeUnit.MILLISECONDS).min(window.value))
    lastRound  = Some(now)
    val budget = new LookupFillBudget(at.allowanceOver(span), at.pace, sleep)
    // Live within the budget, every answer normalized into the model's TMDB store — where the model's
    // gaps are, by definition, not yet.
    val LookupFillChain.Round(client, lookups, gaps) = chain.over(budget)
    // Exactly the questions the identity model found unanswered, and the records it lacks — never
    // a walk of every listing's questions: the model knows its gaps. A record a newly answered
    // search names is the model's gap after its next drain, and the next round's question.
    val asked = ledger.due(questions())
    val outcomes = tools.HttpPath.within(tools.HttpPath.Fill)(
      asked.queries.toSeq.sorted.map(q => Question.Candidate(q) -> ledger.asked(budget)(lookups.candidates(q))) ++
        asked.films.toSeq.sorted.map(id => Question.FilmRecord(id) -> ledger.asked(budget)(lookups.film(id))))
    ledger.remember(outcomes, budget)
    renew(lookups, client, budget)
    ledger.roundEnded(budget)
    current = Some(if (budget.paceOverloaded) at.halved else IdentityLookupFillRate((at.perMinute * 2).min(rate.perMinute)))
    val r = LookupFillRound(budget.asked.get, budget.answered.get, budget.failed.get, budget.deferred.get, gaps.total,
      budget.backedOff, at)
    metrics.round(r)
    ticks.completed(LookupFill.Tick, window.value)
    logger.info(s"identity lookup fill: asked ${r.asked} (${r.answered} answered, ${r.failed} failed), deferred ${r.deferred} " +
      s"at ${at.perMinute}/min${if (r.backedOff) s" — backed off ${budget.overloadedHosts.toSeq.sorted.mkString(", ")}" else ""}")
    r
  }

  /** With what the allowance has left, the aged questions, records and people asked again ([[QuestionRenewals]]). */
  private def renew(lookups: TmdbIdentityLookups, client: TmdbClient, budget: LookupFillBudget): Unit =
    tools.HttpPath.within(tools.HttpPath.Refresh)(renewals().iterator.takeWhile(_ => budget.remaining > 0).foreach {
      case Question.Candidate(query) => lookups.candidates(query)
      case Question.FilmRecord(id)   => try client.filmAnswers(id) catch { case NonFatal(_) => () }
      case Question.Person(id)       => try client.personDirectorCredits(id) catch { case NonFatal(_) => () }
      case _                         => ()
    })

  /** Start a round in the background unless one is running. A worker's shutdown interrupts a round
   *  mid-sleep or mid-fetch: that ends it, interrupt re-asserted, rather than escaping `NonFatal` as an
   *  uncaught exception Sentry reports as FATAL on every restart. */
  def start(): Unit =
    if (running.compareAndSet(false, true))
      executor.execute { () =>
        try round() catch {
          case _: InterruptedException => Thread.currentThread().interrupt()
          case NonFatal(e)             => logger.warn("identity lookup fill: round failed", e)
        }
        finally running.set(false)
      }
}

object LookupFill {
  /** The fill's tick: armed by its schedule ([[LookupFillSchedule]]), completed by each round that ran to its end. */
  val Tick = "identity-lookup-refresh"

  /** What one asked gap came to in a round. */
  private[services] sealed trait Asked
  private[services] object Asked {
    /** Deferred for want of budget: nothing to remember. */
    case object Settled    extends Asked
    /** Answered: a run of failures it had is over, and counts again from none. */
    case object Answered   extends Asked
    /** TMDB answered and left it unanswered (a 404, nothing the store takes): the day's wait. */
    case object Unanswered extends Asked
    /** A read it took failed (5xx, timeout, 429, open breaker): the short failure backoff. */
    case object Failed     extends Asked
  }
}
