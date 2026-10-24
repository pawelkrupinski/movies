package services.identity.adapters

import services.freshness.{Freshness, FreshnessKind}
import services.identity.QuestionKind
import services.identity.agreement.AgreementStage
import services.venuepages.MongoVenuePageStore

import scala.concurrent.duration._

/**
 * What each kind of identity question is held to, in one table: where its answer is stored, when it is asked again,
 * what is remembered when the source gives no answer, how long an unread answer is kept, and how long a take waits on
 * it. The table decides: every age, wait and sweep below is defined here and nowhere else, and the code that acts on one
 * reads it from here — the renewals ([[QuestionRenewals]]), the non-answer memory ([[QuestionMemory]], by the
 * question's kind), the agreement's stores' freshness (`wanted`) and the store sweep ([[TmdbStoreSweep]])
 * (docs/design/identity-questions.md holds it as prose, [[QuestionPoliciesSpec]] keeps the two in step).
 *
 * One value is not the identity system's own: a venue's detail page is read by the pipeline, on the detail refresh's
 * age (`Freshness` DetailEnrich), which the table only states.
 */
object QuestionPolicies {

  // ── The model's questions ─────────────────────────────────────────────────────────────────────
  /** A decision this sure of its film is settled: its questions are asked again only weekly. */
  val Settled: Double = 0.9
  /** A question of a family the model has not settled is asked again a day after it was last asked. */
  val UnsettledAge: FiniteDuration = 1.day
  /** A settled question a week after: a new film reaches every stored search within a week. */
  val SettledAge: FiniteDuration = 7.days
  /** A film's or person's record the model reads is asked again this long after TMDB last gave it, whatever the changes
   *  feed named: as long as an unread one is kept ([[KeepUnread]]). */
  val ModelRecordAge: FiniteDuration = 30.days

  // ── The non-answer memory ([[QuestionMemory]]) ──────────────────────────────────────────────────
  /** How long a question its source durably left unanswered (TMDB's 404/410, an answer the store takes nothing from)
   *  waits before it is asked again. */
  val UnansweredWait: FiniteDuration = 1.day
  /** The first wait after a failed ask, doubled per further failure up to [[MaxFailureWait]]. */
  val FirstFailureWait: FiniteDuration = 5.minutes
  val MaxFailureWait: FiniteDuration   = 6.hours

  // ── The agreement's questions ─────────────────────────────────────────────────────────────────
  /** How long a family's search is read before it is asked again: venues list a title for weeks, and a new film
   *  joining a search is what a re-ask would find. */
  val FamilySearchAge: FiniteDuration = 90.days
  /** How long a family's record of a film is read before it is asked again: a released film's facts do not move. */
  val FamilyRecordAge: FiniteDuration = 365.days
  /** How long a venue's programme on a family's site is read before it is asked again: a day's, as the programme moves. */
  val FamilyShowingAge: FiniteDuration = 20.hours
  /** How long a poster's hash is read before the poster is hashed again. */
  val PosterAge: FiniteDuration = 365.days
  /** How long a poster not read — given up on, or refused — is read as unread before it is tried again. */
  val PosterUnreadAge: FiniteDuration = 7.days
  /** The failed fetch on which a poster that keeps failing is given up on (Kino Kryterium's posters, 2026-10-06: ten
   *  attempts between 14:55 and 16:09). */
  val PosterGiveUpAttempts: Int = 8
  /** How long a catalogue id's mapping to a film is read before it is asked again. */
  val CatalogueMappedAge: FiniteDuration = 365.days
  /** How long a catalogue id Wikidata states on no item yet is read before it is asked again: it gains items. */
  val CatalogueUnmappedAge: FiniteDuration = 30.days
  /** How long a catalogue page's links are read before it is read again. */
  val CataloguePageAge: FiniteDuration = 90.days

  // ── The store sweep ([[TmdbStoreSweep]]) ──────────────────────────────────────────────────────
  /** How long an answer the model no longer reads is kept since TMDB last gave it: a listing that comes back within a
   *  month is answered from the store, not asked again. */
  val KeepUnread: FiniteDuration = 30.days
  /** How long another family's answer is kept since it was last fetched: longer than [[FamilyRecordAge]], so a record
   *  is asked again before it can be swept. */
  val KeepFamily: FiniteDuration = 400.days
  /** How long a non-answer marker is kept since it was stamped: a week of 1-day waits' worth, and every wait's end. */
  val MarkerGrace: FiniteDuration = 7.days

  /** When a stored answer is asked again: past an age (`when` says which answers), or when a feed names it. */
  enum Renewal {
    case After(age: FiniteDuration, when: String)
    /** TMDB's daily change lists (`TmdbChangesSweep`). */
    case ChangesFeed
  }

  /** What a source giving no answer leaves behind. Every kind the identity system asks itself is remembered by
   *  [[QuestionMemory]] (an `unanswered|` marker in `identity_question_markers`, swept [[MarkerGrace]] after it was
   *  stamped): a failed ask waits `firstFailure`, doubling while it keeps failing, up to `maxFailure`. */
  enum NonAnswer {
    /** A durable non-answer remembered too, waiting `unanswered`: the model's questions, whose stores take no empty
     *  answer. */
    case Remembered(unanswered: FiniteDuration, firstFailure: FiniteDuration, maxFailure: FiniteDuration)
    /** An empty answer is filed as one ("no film"), read as the store's answer; a failure remembered. */
    case EmptyFiled(firstFailure: FiniteDuration, maxFailure: FiniteDuration)
    /** A failure remembered, and on the `attempts`th in a row given up on: filed as unread and remembered `retry`. */
    case GivenUp(firstFailure: FiniteDuration, maxFailure: FiniteDuration, attempts: Int, retry: FiniteDuration)
    /** Read within a director search's walk: that question's marker holds it back. */
    case WithItsSearch
    /** Not the identity system's ask: the pipeline files a gone page (404/410), marks a failed read attempted
     *  (`EnrichDetailsTasks.pageAttempted`) and reads it again past the detail refresh's age, one the store did not take
     *  on the task queue's backoff. */
    case GoneFiled

    /** How long [[QuestionMemory]] holds a question back once it is remembered as durably unanswered — or, for a kind
     *  given up on, as given up — if it remembers that at all. */
    def unansweredWait: Option[FiniteDuration] = this match {
      case Remembered(wait, _, _)                       => Some(wait)
      case GivenUp(_, _, _, retry)                      => Some(retry)
      case EmptyFiled(_, _) | WithItsSearch | GoneFiled => None
    }

    /** The first and the longest wait [[QuestionMemory]] holds a failed question back, if it remembers failures. */
    def failureWaits: Option[(FiniteDuration, FiniteDuration)] = this match {
      case Remembered(_, first, max) => Some(first -> max)
      case EmptyFiled(first, max)    => Some(first -> max)
      case GivenUp(first, max, _, _) => Some(first -> max)
      case WithItsSearch | GoneFiled => None
    }
  }

  /** When the store sweep ([[TmdbStoreSweep]]) deletes an answer. */
  enum Sweep {
    /** Unread by the model taken up, and unfetched, this long. */
    case Unread(after: FiniteDuration)
    /** Unfetched this long, read or not. */
    case Fetched(after: FiniteDuration)
    /** Never. */
    case Kept
  }

  final case class Policy(
    store: String,
    renewal: Seq[Renewal],
    nonAnswer: NonAnswer,
    swept: Sweep,
    /** How long a take waits on an open question of this kind before deciding without it; `None`: no wait of its own
     *  (the model's gaps wait on the lookup fill's rounds). */
    waitedOn: Option[FiniteDuration]
  )

  private val remembered = NonAnswer.Remembered(UnansweredWait, FirstFailureWait, MaxFailureWait)
  private val emptyFiled = NonAnswer.EmptyFiled(FirstFailureWait, MaxFailureWait)

  private val search = Policy(
    TmdbKind.Query.collection,
    Seq(Renewal.After(UnsettledAge, s"its cluster below $Settled confidence"), Renewal.After(SettledAge, "settled")),
    remembered,
    Sweep.Unread(KeepUnread),
    None
  )

  private def record(store: String, nonAnswer: NonAnswer) =
    Policy(store, Seq(Renewal.ChangesFeed, Renewal.After(ModelRecordAge, "read by the model")), nonAnswer, Sweep.Unread(KeepUnread), None)

  private def agreement(store: String, renewal: Renewal*) =
    Policy(store, renewal, emptyFiled, Sweep.Fetched(KeepFamily), Some(AgreementStage.PartialAfter))

  private def family(age: FiniteDuration) = agreement(TmdbKind.Family.collection, Renewal.After(age, "any"))

  val table: Map[QuestionKind, Policy] = Map(
    QuestionKind.Title       -> search,
    QuestionKind.Director    -> search,
    QuestionKind.Imdb        -> search,
    QuestionKind.ImdbTitled  -> search,
    QuestionKind.TitleCredit -> search,
    QuestionKind.FilmRecord  -> record(TmdbKind.Film.collection, remembered),
    QuestionKind.Person      -> record(TmdbKind.Person.collection, NonAnswer.WithItsSearch),
    QuestionKind.VenuePage -> Policy(
      MongoVenuePageStore.Collection,
      Seq(Renewal.After(Freshness.ttlFor(FreshnessKind.DetailEnrich).get, "any")),
      NonAnswer.GoneFiled,
      Sweep.Kept,
      None
    ),
    QuestionKind.FamilySearch  -> family(FamilySearchAge),
    QuestionKind.FamilyRecord  -> family(FamilyRecordAge),
    QuestionKind.FamilyShowing -> family(FamilyShowingAge),
    // TMDB's answers, filed in the TMDB store as the model's are, through the TMDB client: a find naming no film is filed
    // as one (`TmdbStore.foundFirst`), a record TMDB does not know as empty. A record read again is asked once in the
    // stage's life, when a take waits on its release day.
    QuestionKind.Find   -> agreement(TmdbKind.Query.collection).copy(swept = Sweep.Unread(KeepUnread)),
    QuestionKind.Record -> agreement(TmdbKind.Film.collection).copy(swept = Sweep.Unread(KeepUnread)),
    QuestionKind.Poster -> agreement(TmdbKind.Family.collection, Renewal.After(PosterAge, "hashed"))
      .copy(nonAnswer = NonAnswer.GivenUp(FirstFailureWait, MaxFailureWait, PosterGiveUpAttempts, PosterUnreadAge)),
    QuestionKind.CataloguePage -> family(CataloguePageAge),
    QuestionKind.CatalogueId -> agreement(
      TmdbKind.Family.collection,
      Renewal.After(CatalogueMappedAge, "mapped to a film"),
      Renewal.After(CatalogueUnmappedAge, "mapped to none")
    )
  )

  def of(kind: QuestionKind): Policy = table(kind)
}
