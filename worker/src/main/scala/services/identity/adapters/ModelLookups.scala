package services.identity
package adapters

import services.cinemas.common.DetailEnricher

/**
 * The identity model's venue detail and nothing else: a listing's page as venue_pages holds it
 * ([[VenuePageIndex]]) — a gap until the page is read, re-asked when its read is announced. TMDB and IMDb are the store's ([[StoredTmdbLookups]], whose `details` this is).
 */
final class VenueDetailLookups(enrichers: Seq[DetailEnricher], index: VenuePageIndex,
                               reads: ObservationReads = ObservationReads.Untracked) extends IdentityLookups {
  private val gaps    = new LookupGaps
  private val details = new VenueDetails(enrichers.map(new VenuePageDetailEnricher(_, index, gaps, reads)), gaps)

  def hasDetail(listing: Listing): Boolean                  = details.hasDetail(listing)
  def detail(listing: Listing): Answer[Option[DetailFacts]] = details.detail(listing)
  def candidates(query: CandidateQuery): Answer[Seq[Hit]]   = Answer.Unknown
  def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Unknown
}

/**
 * The model's lookups answered from what it keeps first (`stored`: its normalized TMDB store and the
 * venues' enriched details), and only a TMDB or IMDb question the store has no answer to asked `live`
 * — through the fetch that files its answer into the store (`NormalizingHttpFetch`), then read back from
 * there. So a take-up reads the store in batches instead of asking every question again; venue details
 * are never fetched here, only waited for. A live ask is the `live` path of `kinowo_worker_http_total`.
 *
 * Eager for what is new, the rest the fill's (docs/design/identity-questions.md, step 3): a question asked
 * live and left unanswered is remembered (`asks`, the one [[QuestionMemory]]), and while it waits out
 * its backoff it is not asked live again — the model reads it as the gap it is, and the paced fill asks
 * it when it is due. A newly scraped film's questions, never asked, are asked at once.
 */
final class StoredFirstLookups(stored: IdentityLookups, live: IdentityLookups, asks: LiveAsks = LiveAsks.Always)
    extends ForwardingIdentityLookups(stored) {
  override def candidates(query: CandidateQuery): Answer[Seq[Hit]] =
    storedFirst(Question.Candidate(query), stored.candidates(query), live.candidates(query))
  override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] =
    storedFirst(Question.FilmRecord(tmdbId), stored.film(tmdbId), live.film(tmdbId))
  /** The store's day of a record it holds — `Unknown` for one it holds dated by its year alone, which the agreement
   *  stage asks TMDB to read again (`agreement.AgreementStage.wanted`, its `Question.Record`s) — else the day of the record asked live. */
  override def releaseDay(tmdbId: Int): Answer[Option[java.time.LocalDate]] = film(tmdbId) match {
    case Answer.Known(_) if stored.film(tmdbId).isKnown => stored.releaseDay(tmdbId)
    case answer                                         => answer.map(_.flatMap(_.released))
  }
  // a film's facts, cast and overview are one record: one question
  override def cast(tmdbId: Int): Answer[Option[Seq[String]]] =
    storedFirst(Question.FilmRecord(tmdbId), stored.cast(tmdbId), live.cast(tmdbId))
  override def overview(tmdbId: Int): Answer[Option[ProseWords]] =
    storedFirst(Question.FilmRecord(tmdbId), stored.overview(tmdbId), live.overview(tmdbId))

  /** The store's answer; on a gap, the live one -- read back from the store it was just filed into, so the question
   *  answers as the store holds it (popularity to its bucket), as every later ask of it will: answered as TMDB stated
   *  it, a take-up decided on one input and its next drain on another. The live answer only when the store took none.
   *  A gap still in its backoff is not asked; one asked and left unanswered is remembered, failed or not — a failure
   *  against the host its read went to ([[LiveAsks.failedHost]]: an IMDb suggestion's is IMDb's), whose recovery releases it. */
  private def storedFirst[A](question: Question, fromStore: => Answer[A], asked: => Answer[A]): Answer[A] =
    unlessGivenUp(question, fromStore.orElse {
      if (!asks.due(question)) Answer.Unknown
      else {
        val failedBefore = LiveAsks.failures
        val requested    = new tools.RequestedHost.Recording
        val liveAnswer   = tools.RequestedHost.within(requested)(tools.HttpPath.within(tools.HttpPath.Live)(asked))
        val answer       = fromStore.orElse(liveAnswer)
        if (!answer.isKnown) {
          val failed = LiveAsks.failures != failedBefore
          asks.unanswered(question, failed, if (failed) LiveAsks.failedHost(requested) else None)
        }
        answer
      }
    })

  /** A question still unanswered that the memory has GIVEN UP on (failed its kind's give-up count in a row): `Failed`,
   *  decided on as no evidence, never awaited (docs/design/identity-structural-fixes.md fix 4b; the owner, 2026-10-31: a
   *  take is settled once every question is answered or given up on). Asked again on its backoff all the same. */
  private def unlessGivenUp[A](question: Question, answer: Answer[A]): Answer[A] =
    if (answer == Answer.Unknown && asks.givenUp(question)) Answer.Failed else answer
}

/** Which of the model's gaps it asks live, and what it remembers of those left unanswered. */
trait LiveAsks {
  /** Is `question` asked live now — never asked, or past its backoff? */
  def due(question: Question): Boolean
  /** `question` was asked live and left unanswered: a durable non-answer, or a read that `failed` — against `host`, when
   *  known. */
  def unanswered(question: Question, failed: Boolean, host: Option[String]): Unit
  /** Has `question` been asked and given up on: its last asks failed its kind's give-up count in a row? Never for one
   *  never attempted. */
  def givenUp(question: Question): Boolean
}

object LiveAsks {

  /** Every gap asked live, nothing remembered: what the model did before step 3, and a replay's lookups. */
  val Always: LiveAsks = new LiveAsks {
    def due(question: Question): Boolean                       = true
    def unanswered(question: Question, failed: Boolean, host: Option[String]): Unit = ()
    def givenUp(question: Question): Boolean = false
  }

  /** The one memory ([[QuestionMemory]]): one backoff for a gap, whoever asked it. */
  def remembered(memory: QuestionMemory): LiveAsks = new LiveAsks {
    def due(question: Question): Boolean = memory.due(Set(question)).nonEmpty
    def unanswered(question: Question, failed: Boolean, host: Option[String]): Unit =
      if (failed) { memory.failed(Seq(question), host); () } else memory.unanswered(Seq(question))
    def givenUp(question: Question): Boolean = memory.outcome(question) match {
      case Some(QuestionMemory.Outcome.Failed(times, _, _)) => times >= QuestionPolicies.giveUpFailures(question.kind)
      case _                                               => false
    }
  }

  private val failed      = ThreadLocal.withInitial[java.lang.Long](() => 0L)
  private val lastFailure = new ThreadLocal[Throwable]
  /** How many live reads failed on this thread (a 5xx, a timeout, a 429, an open breaker; not a 404): what tells a
   *  live ask's failure from TMDB's durable non-answer. */
  def failures: Long = failed.get

  /** The host this thread's latest failed read went to: the one its failure names, else the last host requested within
   *  `requested` ([[AgreementQuestions.hostOf]], as the agreement's asks name theirs). */
  def failedHost(requested: tools.RequestedHost.Recording): Option[String] = AgreementQuestions.hostOf(lastFailure.get, requested.last)

  /** `fetch`, its failed reads counted on the requesting thread ([[failures]]) and each request's host noted
   *  (`tools.RequestedHost`). */
  final class Counted(fetch: tools.HttpFetch) extends tools.ForwardingHttpFetch {
    protected def around[T](url: String)(call: tools.HttpFetch => T): T =
      try { tools.RequestedHost.note(LookupFillLiveFetch.hostOf(url)); call(fetch) }
      catch {
        case scala.util.control.NonFatal(e) if !tools.ReadOutcome.isAbsent(e) =>
          failed.set(failed.get + 1); lastFailure.set(e); throw e
      }
  }
}
