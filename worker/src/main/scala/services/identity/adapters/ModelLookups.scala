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
 * live and left unanswered is remembered (`asks`, the fill's [[TmdbGapMemory]]), and while it waits out
 * its backoff it is not asked live again — the model reads it as the gap it is, and the paced fill asks
 * it when it is due. A newly scraped film's questions, never asked, are asked at once.
 */
final class StoredFirstLookups(stored: IdentityLookups, live: IdentityLookups, asks: LiveAsks = LiveAsks.Always)
    extends ForwardingIdentityLookups(stored) {
  import LiveAsks.Key
  override def candidates(query: CandidateQuery): Answer[Seq[Hit]] =
    storedFirst(Key.Query(query), stored.candidates(query), live.candidates(query))
  override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = storedFirst(Key.Film(tmdbId), stored.film(tmdbId), live.film(tmdbId))
  /** The store's day of a record it holds — `Unknown` for one it holds dated by its year alone, which the agreement
   *  stage asks TMDB to read again (`agreement.AgreementStage.wantedRecords`) — else the day of the record asked live. */
  override def releaseDay(tmdbId: Int): Answer[Option[java.time.LocalDate]] = film(tmdbId) match {
    case Answer.Known(_) if stored.film(tmdbId).isKnown => stored.releaseDay(tmdbId)
    case answer                                         => answer.map(_.flatMap(_.released))
  }
  override def cast(tmdbId: Int): Answer[Option[Seq[String]]] = storedFirst(Key.Film(tmdbId), stored.cast(tmdbId), live.cast(tmdbId))
  override def overview(tmdbId: Int): Answer[Option[ProseWords]] = storedFirst(Key.Film(tmdbId), stored.overview(tmdbId), live.overview(tmdbId))

  /** The store's answer; on a gap, the live one -- read back from the store it was just filed into, so the question
   *  answers as the store holds it (popularity to its bucket), as every later ask of it will: answered as TMDB stated
   *  it, a take-up decided on one input and its next drain on another. The live answer only when the store took none.
   *  A gap still in its backoff is not asked; one asked and left unanswered is remembered, failed or not. */
  private def storedFirst[A](key: Key, fromStore: => Answer[A], asked: => Answer[A]): Answer[A] =
    fromStore.orElse {
      if (!asks.due(key)) Answer.Unknown
      else {
        val failedBefore = LiveAsks.failures
        val liveAnswer   = tools.HttpPath.within(tools.HttpPath.Live)(asked)
        val answer       = fromStore.orElse(liveAnswer)
        if (!answer.isKnown) asks.unanswered(key, failed = LiveAsks.failures != failedBefore)
        answer
      }
    }
}

/** Which of the model's gaps it asks live, and what it remembers of those left unanswered. */
trait LiveAsks {
  /** Is `key` asked live now — never asked, or past its backoff? */
  def due(key: LiveAsks.Key): Boolean
  /** `key` was asked live and left unanswered: a durable non-answer, or a read that `failed`. */
  def unanswered(key: LiveAsks.Key, failed: Boolean): Unit
}

object LiveAsks {
  enum Key {
    case Query(query: CandidateQuery)
    /** A film's record: its facts, cast and overview are one record. */
    case Film(tmdbId: Int)
  }

  /** Every gap asked live, nothing remembered: what the model did before step 3, and a replay's lookups. */
  val Always: LiveAsks = new LiveAsks {
    def due(key: Key): Boolean                       = true
    def unanswered(key: Key, failed: Boolean): Unit = ()
  }

  /** The fill's memory ([[TmdbGapMemory]]): one backoff for a gap, whoever asked it. */
  def remembered(memory: TmdbGapMemory): LiveAsks = new LiveAsks {
    def due(key: Key): Boolean = key match {
      case Key.Query(q)  => memory.due(AnswersChanged(Set(q), Set.empty)).queries.nonEmpty
      case Key.Film(id)  => memory.due(AnswersChanged(Set.empty, Set(id))).films.nonEmpty
    }
    def unanswered(key: Key, failed: Boolean): Unit = {
      val (queries, films) = key match {
        case Key.Query(q) => (Seq(q), Nil)
        case Key.Film(id) => (Nil, Seq(id))
      }
      if (failed) memory.failed(queries, films) else memory.unanswered(queries, films)
    }
  }

  private val failed = ThreadLocal.withInitial[java.lang.Long](() => 0L)
  /** How many live reads failed on this thread (a 5xx, a timeout, a 429, an open breaker; not a 404): what tells a
   *  live ask's failure from TMDB's durable non-answer. */
  def failures: Long = failed.get

  /** `fetch`, its failed reads counted on the requesting thread ([[failures]]). */
  final class Counted(fetch: tools.HttpFetch) extends tools.ForwardingHttpFetch {
    protected def around[T](url: String)(call: tools.HttpFetch => T): T =
      try call(fetch)
      catch {
        case scala.util.control.NonFatal(e) if !tools.ReadOutcome.isAbsent(e) => failed.set(failed.get + 1); throw e
      }
  }
}
